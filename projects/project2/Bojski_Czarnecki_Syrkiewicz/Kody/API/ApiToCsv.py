import requests
import csv
from datetime import datetime
import math
import pandas as pd
def timestamp_to_date(timestamp):
    try:
        if timestamp > 10**10:
            timestamp /= 1000
        return datetime.fromtimestamp(timestamp).strftime("%Y-%m-%d %H:%M:%S")
    except Exception as e:
        return f"Error: {e}"




def main():
    api_key = "RGAPI-9a1b92d3-bc61-44a5-9ced-40373f410928"
    match_list_api_url = "https://europe.api.riotgames.com/lol/match/v5/matches/by-puuid/ExxYz2SH5JqVCFUxCloOfxs0KMCCsbC9O3XPCXu2ZL3MWFOu1ru5odV6jU1i5us8_VzHwnKyGQGyPg/ids?start=0&count=100"
    match_list_api_url = match_list_api_url + '&api_key=' + api_key

    resp = requests.get(match_list_api_url)
    match_ids = resp.json()

    with open('ProGracz_1.csv', mode='w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file)
        writer.writerow(["Data","Champion", "Position", "Kills", "Deaths", "Assists", "KDA", "Win","Damage","Vision","Gold",'cs','Kp','damage%','flash'])

        for match_id in match_ids:
            match_api_url = f"https://europe.api.riotgames.com/lol/match/v5/matches/{match_id}?api_key={api_key}"
            resp = requests.get(match_api_url)

            if resp.status_code != 200:
                print(f"Failed to fetch data for match {match_id}, skipping...")
                continue

            match_data = resp.json()
            game_duration=match_data['info']['gameDuration']/60

            participants = match_data['metadata']['participants']
            player_index = participants.index("ExxYz2SH5JqVCFUxCloOfxs0KMCCsbC9O3XPCXu2ZL3MWFOu1ru5odV6jU1i5us8_VzHwnKyGQGyPg")
            player_data = match_data['info']['participants'][player_index]
            date = timestamp_to_date(match_data['info']['gameStartTimestamp'])
            name = player_data['championName']
            position = player_data['teamPosition']
            k = player_data['kills']
            d = player_data['deaths']
            a = player_data['assists']
            win = player_data['win']
            damage = player_data['totalDamageDealtToChampions']
            wizja = player_data['visionScore']
            zloto_per_minute = round(player_data['goldEarned'] / game_duration, 3)
            kda = round((k + a) / d, 3) if d > 0 else "Perfect KDA"
            cs_per_minute = round(player_data['totalMinionsKilled'] / game_duration, 3)
            kp = round(player_data.get('challenges', {}).get('killParticipation', math.nan), 3) if isinstance(
                player_data.get('challenges', {}).get('killParticipation', math.nan), (int, float)) else math.nan
            damageprocent = round(player_data.get('challenges', {}).get('teamDamagePercentage', math.nan), 3) if isinstance(
                player_data.get('challenges', {}).get('teamDamagePercentage', math.nan), (int, float)) else math.nan
            flash = round(player_data.get('challenges', {}).get('multikillsAfterAggressiveFlash', math.nan),
                          3) if isinstance(
                player_data.get('challenges', {}).get('multikillsAfterAggressiveFlash', math.nan),
                (int, float)) else math.nan

            writer.writerow([date, name, position, k, d, a, kda, win,damage,wizja,zloto_per_minute, cs_per_minute,kp,damageprocent ,flash  ])
    print("Data saved to 'match_statistics.csv'")


if __name__ == '__main__':
    main()

