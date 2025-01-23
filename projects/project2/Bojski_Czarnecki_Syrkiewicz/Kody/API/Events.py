import requests
import pandas as pd

# Konfiguracja
API_KEY = "RGAPI-cf78151b-c0f8-4631-ad98-9edc87ba8cdb"
PUUID = "gvoAqYp_sGkRdYvAy4SZD98TIy4KHMvrlH3GcYKcL4RZvrtT6UWK9WD4qJmRUL2p7u29gdGpifKVXw"
BASE_URL = "https://europe.api.riotgames.com"
MATCH_IDS_URL = f"{BASE_URL}/lol/match/v5/matches/by-puuid/{PUUID}/ids?start=0&count=100&api_key={API_KEY}"
OUTPUT_FILE = "../Projekt_2_TWD/API/Bottom_matches.csv"

# Pobranie listy meczów
response = requests.get(MATCH_IDS_URL)
if response.status_code != 200:
    raise Exception(f"Failed to fetch match IDs: {response.status_code}")

match_ids = response.json()

if not match_ids:
    raise Exception("No match IDs found")

# Wybieramy 3 pierwsze mecze
selected_matches = match_ids[7:10]

# Lista na dane wyjściowe
data_records = []

# Przetwarzanie wybranych meczów
for match_id in selected_matches:
    match_url = f"{BASE_URL}/lol/match/v5/matches/{match_id}?api_key={API_KEY}"
    timeline_url = f"{BASE_URL}/lol/match/v5/matches/{match_id}/timeline?api_key={API_KEY}"

    match_response = requests.get(match_url)
    timeline_response = requests.get(timeline_url)

    if match_response.status_code == 200 and timeline_response.status_code == 200:
        match_data = match_response.json()
        timeline_data = timeline_response.json()

        # Znajdź indeks gracza
        player_idx = next((i for i, p in enumerate(match_data['metadata']['participants']) if p == PUUID), None)

        if player_idx is not None:
            participant = match_data['info']['participants'][player_idx]

            if participant['teamPosition'] == "BOTTOM":  # Sprawdzamy, czy gracz grał na pozycji "MIDDLE"
                champion_name = participant['championName']
                champion_id = participant['championId']

                # Pobieranie wydarzeń z timeline
                for frame in timeline_data['info']['frames']:
                    for event in frame['events']:
                        position = event.get('position', {})
                        x = position.get('x', None)
                        y = position.get('y', None)

                        if x is not None and y is not None:
                            event_type = None
                            if event.get('killerId') == player_idx + 1:
                                event_type = "kill"
                            elif event.get('victimId') == player_idx + 1:
                                event_type = "death"
                            elif event.get('assistingParticipantIds') and (player_idx + 1) in event['assistingParticipantIds']:
                                event_type = "assist"

                            if event_type:
                                data_records.append({
                                    "player_id": PUUID,
                                    "match_id": match_id,
                                    "x": x,
                                    "y": y,
                                    "type": event_type,
                                    "minute": frame['timestamp'] // 60000,
                                    "timestamp": event.get('timestamp'),
                                    "champion_id": champion_id,
                                    "champion_name": champion_name,
                                })

# Zapisz dane do pliku CSV
df = pd.DataFrame(data_records)
df.to_csv(OUTPUT_FILE, index=False)

print(f"Data saved to {OUTPUT_FILE}")
