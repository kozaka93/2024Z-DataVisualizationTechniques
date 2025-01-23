import pandas as pd

def add_place_names(name):
    df = pd.read_csv(f"../data/places/places_{name}.csv")
    df.drop(columns=["latitude", "longitude"], inplace=True)

    map = pd.read_csv(f"../data/places_mapping_{name}.csv")

    df_merged = df.merge(map, on="placeID", how="left")
    df_merged["person"] = name

    return df_merged

def merge_csv_with_place_names():
    df_kl = add_place_names("kl")
    df_jo = add_place_names("jo")
    df_mi = add_place_names("mi")

    df = pd.concat([df_kl, df_jo, df_mi])

    df.drop_duplicates(inplace=True)

    df.to_csv("../data/merged_data.csv", index=False)

if __name__ == '__main__':
    merge_csv_with_place_names()
