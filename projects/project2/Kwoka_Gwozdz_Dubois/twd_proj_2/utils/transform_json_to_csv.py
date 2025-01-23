import pandas as pd
import json
from datetime import datetime, timedelta

def transform_place_location(place_location):
    # if place location is str
    if isinstance(place_location, str):
        place_location = place_location.replace("geo:", "")

    elif isinstance(place_location, dict):
        place_location = place_location["latLng"].replace("\u00b0", "")

    lat, lon = place_location.split(",")
    return lat.strip(), lon.strip()

def json_to_dataframe_places(data):
    rows = []
    for entry in data:
        try:
            place_location = entry["visit"]["topCandidate"]["placeLocation"]
            lat, lon = transform_place_location(place_location)
            row = {
                "endTime": entry["endTime"],
                "startTime": entry["startTime"],
                "hierarchyLevel": int(entry["visit"]["hierarchyLevel"]),
                "visitProbability": float(entry["visit"]["probability"]),
                "topCandidateProbability": float(entry["visit"]["topCandidate"]["probability"]),
                "semanticType": entry["visit"]["topCandidate"]["semanticType"],
                "placeID": entry["visit"]["topCandidate"].get("placeID", entry["visit"]["topCandidate"].get("placeId")),
                "latitude": lat,
                "longitude": lon,
            }
            rows.append(row)
        except (KeyError, TypeError, ValueError):
            continue

    df = pd.DataFrame(rows)
    start_date = datetime(2024, 12, 9)

    if not df.empty:
        df["startTime"] = pd.to_datetime(df["startTime"]).dt.tz_localize(None)

        df["relativeWeekNum"] = ((pd.to_datetime(df["startTime"]) - start_date).dt.days // 7) + 1

    return df

def json_to_dataframe_activities(data):
    rows = []
    for entry in data:
        try:
            activity = entry["activity"]
            start_lat, start_lon = transform_place_location(activity["start"])
            end_lat, end_lon = transform_place_location(activity["end"])

            row = {
                "endTime": entry["endTime"],
                "startTime": entry["startTime"],
                "activityProbability": float(activity["probability"]),
                "topCandidateType": activity["topCandidate"]["type"],
                "topCandidateProbability": float(activity["topCandidate"]["probability"]),
                "distanceMeters": float(activity["distanceMeters"]),
                "startLatitude": start_lat,
                "startLongitude": start_lon,
                "endLatitude": end_lat,
                "endLongitude": end_lon,
            }
            rows.append(row)
        except (KeyError, TypeError, ValueError):
            continue

    df = pd.DataFrame(rows)
    start_date = datetime(2024, 12, 9)

    if not df.empty:
        df["startTime"] = pd.to_datetime(df["startTime"]).dt.tz_localize(None)
        df["relativeWeekNum"] = ((pd.to_datetime(df["startTime"]) - start_date).dt.days // 7) + 1

    return df

def transform_to_csv(filename):
    with open(f"../data/Os_czasu_{filename}.json", 'r') as file:
        data = json.load(file)

    df_places = json_to_dataframe_places(data)
    df_places.to_csv(f"../data/places/places_{filename}.csv", index=False)

    df_travel = json_to_dataframe_activities(data)
    df_travel.to_csv(f"../data/travel/travel_{filename}_travel.csv", index=False)


if __name__ == '__main__':
    transform_to_csv('kl')
    transform_to_csv('jo')
    transform_to_csv('mi')




