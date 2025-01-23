import requests
import csv
from datetime import datetime
import math
import pandas as pd
import requests
import csv
import math
from datetime import datetime, timedelta
import time

def timestamp_to_date(timestamp):
    try:
        if timestamp > 10**10:
            timestamp /= 1000
        return datetime.fromtimestamp(timestamp).strftime("%Y-%m-%d %H:%M:%S")
    except Exception as e:
        return f"Error: {e}"

def get_timestamp(year, month, day):
    return int(datetime(year, month, day).timestamp())

def main():
    api_key = "RGAPI-67358834-9b1b-4cc8-b42f-7c34ab5ebca2"
    player_unique_id = "iXlnLnGETPtVPo6WkxoyxNFHCZYO0aJlqV3YrYxkqN5A3RxOKJ1QSLK3cagOfa8AHhUg_igPJCN4Ww"
    csv_file_path = 'RYSZARDNEW.csv'

    stat_names = [
        "12AssistStreakCount",
        "baronBuffGoldAdvantageOverThreshold",
        "controlWardTimeCoverageInRiverOrEnemyHalf",
        "earliestBaron",
        "earliestDragonTakedown",
        "earliestElderDragon",
        "earlyLaningPhaseGoldExpAdvantage",
        "fasterSupportQuestCompletion",
        "fastestLegendary",
        "hadAfkTeammate",
        "highestChampionDamage",
        "highestCrowdControlScore",
        "highestWardKills",
        "junglerKillsEarlyJungle",
        "killsOnLanersEarlyJungleAsJungler",
        "laningPhaseGoldExpAdvantage",
        "legendaryCount",
        "maxCsAdvantageOnLaneOpponent",
        "maxLevelLeadLaneOpponent",
        "mostWardsDestroyedOneSweeper",
        "mythicItemUsed",
        "playedChampSelectPosition",
        "soloTurretsLategame",
        "takedownsFirst25Minutes",
        "teleportTakedowns",
        "thirdInhibitorDestroyedTime",
        "threeWardsOneSweeperCount",
        "visionScoreAdvantageLaneOpponent",
        "InfernalScalePickup",
        "fistBumpParticipation",
        "voidMonsterKill",
        "abilityUses",
        "acesBefore15Minutes",
        "alliedJungleMonsterKills",
        "baronTakedowns",
        "blastConeOppositeOpponentCount",
        "bountyGold",
        "buffsStolen",
        "completeSupportQuestInTime",
        "controlWardsPlaced",
        "damagePerMinute",
        "damageTakenOnTeamPercentage",
        "dancedWithRiftHerald",
        "deathsByEnemyChamps",
        "dodgeSkillShotsSmallWindow",
        "doubleAces",
        "dragonTakedowns",
        "legendaryItemUsed",
        "effectiveHealAndShielding",
        "elderDragonKillsWithOpposingSoul",
        "elderDragonMultikills",
        "enemyChampionImmobilizations",
        "enemyJungleMonsterKills",
        "epicMonsterKillsNearEnemyJungler",
        "epicMonsterKillsWithin30SecondsOfSpawn",
        "epicMonsterSteals",
        "epicMonsterStolenWithoutSmite",
        "firstTurretKilled",
        "firstTurretKilledTime",
        "flawlessAces",
        "fullTeamTakedown",
        "gameLength",
        "getTakedownsInAllLanesEarlyJungleAsLaner",
        "goldPerMinute",
        "hadOpenNexus",
        "immobilizeAndKillWithAlly",
        "initialBuffCount",
        "initialCrabCount",
        "jungleCsBefore10Minutes",
        "junglerTakedownsNearDamagedEpicMonster",
        "kda",
        "killAfterHiddenWithAlly",
        "killedChampTookFullTeamDamageSurvived",
        "killingSprees",
        "killParticipation",
        "killsNearEnemyTurret",
        "killsOnOtherLanesEarlyJungleAsLaner",
        "killsOnRecentlyHealedByAramPack",
        "killsUnderOwnTurret",
        "killsWithHelpFromEpicMonster",
        "knockEnemyIntoTeamAndKill",
        "kTurretsDestroyedBeforePlatesFall",
        "landSkillShotsEarlyGame",
        "laneMinionsFirst10Minutes",
        "lostAnInhibitor",
        "maxKillDeficit",
        "mejaisFullStackInTime",
        "moreEnemyJungleThanOpponent",
        "multiKillOneSpell",
        "multikills",
        "multikillsAfterAggressiveFlash",
        "multiTurretRiftHeraldCount",
        "outerTurretExecutesBefore10Minutes",
        "outnumberedKills",
        "outnumberedNexusKill",
        "perfectDragonSoulsTaken",
        "perfectGame",
        "pickKillWithAlly",
        "poroExplosions",
        "quickCleanse",
        "quickFirstTurret",
        "quickSoloKills",
        "riftHeraldTakedowns",
        "saveAllyFromDeath",
        "scuttleCrabKills",
        "shortestTimeToAceFromFirstTakedown",
        "skillshotsDodged",
        "skillshotsHit",
        "snowballsHit",
        "soloBaronKills",
        "soloKills",
        "stealthWardsPlaced",
        "survivedSingleDigitHpCount",
        "survivedThreeImmobilizesInFight",
        "takedownOnFirstTurret",
        "takedowns",
        "takedownsAfterGainingLevelAdvantage",
        "takedownsBeforeJungleMinionSpawn",
        "takedownsFirstXMinutes",
        "takedownsInAlcove",
        "takedownsInEnemyFountain",
        "teamBaronKills",
        "teamDamagePercentage",
        "teamElderDragonKills",
        "teamRiftHeraldKills",
        "tookLargeDamageSurvived",
        "turretPlatesTaken",
        "turretsTakenWithRiftHerald",
        "turretTakedowns",
        "twentyMinionsIn3SecondsCount",
        "twoWardsOneSweeperCount",
        "unseenRecalls",
        "visionScorePerMinute",
        "wardsGuarded",
        "wardTakedowns",
        "wardTakedownsBefore20M"
    ]
    HEADERS = [
        "allInPings", "assistMePings", "assists", "baronKills", "bountyLevel",
        "champExperience", "champLevel", "championId", "championName", "commandPings",
        "championTransform", "consumablesPurchased", "damageDealtToBuildings", "damageDealtToObjectives",
        "damageDealtToTurrets", "damageSelfMitigated", "deaths", "detectorWardsPlaced",
        "doubleKills", "dragonKills", "eligibleForProgression", "enemyMissingPings",
        "enemyVisionPings", "firstBloodAssist", "firstBloodKill", "firstTowerAssist",
        "firstTowerKill", "gameEndedInEarlySurrender", "gameEndedInSurrender", "holdPings",
        "getBackPings", "goldEarned", "goldSpent", "individualPosition", "inhibitorKills",
        "inhibitorTakedowns", "inhibitorsLost", "item0", "item1", "item2", "item3", "item4",
        "item5", "item6", "itemsPurchased", "killingSprees", "kills", "lane",
        "largestCriticalStrike", "largestKillingSpree", "largestMultiKill", "longestTimeSpentLiving",
        "magicDamageDealt", "magicDamageDealtToChampions", "magicDamageTaken", "missions",
        "neutralMinionsKilled", "needVisionPings", "nexusKills", "nexusTakedowns",
        "nexusLost", "objectivesStolen", "objectivesStolenAssists", "onMyWayPings",
        "participantId", "pentaKills", "perks", "physicalDamageDealt",
        "physicalDamageDealtToChampions", "physicalDamageTaken", "placement",
        "playerSubteamId", "pushPings", "profileIcon", "quadraKills",
        "riotIdGameName", "riotIdTagline", "role", "sightWardsBoughtInGame",
        "spell1Casts", "spell2Casts", "spell3Casts", "spell4Casts", "subteamPlacement",
        "summoner1Casts", "summoner1Id", "summoner2Casts", "summoner2Id", "summonerId",
        "summonerLevel", "summonerName", "teamEarlySurrendered", "teamId",
        "teamPosition", "timeCCingOthers", "timePlayed", "totalAllyJungleMinionsKilled",
        "totalDamageDealt", "totalDamageDealtToChampions", "totalDamageShieldedOnTeammates",
        "totalDamageTaken", "totalEnemyJungleMinionsKilled", "totalHeal",
        "totalHealsOnTeammates", "totalMinionsKilled", "totalTimeCCDealt",
        "totalTimeSpentDead", "totalUnitsHealed", "tripleKills", "trueDamageDealt",
        "trueDamageDealtToChampions", "trueDamageTaken", "turretKills", "turretTakedowns",
        "turretsLost", "unrealKills", "visionScore", "visionClearedPings",
        "visionWardsBoughtInGame", "wardsKilled", "wardsPlaced", "win"
    ]

    csv_header = [
        "Data", "Champion", "Position", "Kills", "Deaths", "Assists",
        "KDA", "Win", "Damage", "Vision", "Gold", "cs",
        "Kp", "damage%", "flash"
    ] + stat_names + HEADERS

    with open(csv_file_path, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file)
        writer.writerow(csv_header)

    date_ranges = [
        (datetime(2024, 9, 1), datetime(2024, 9, 30)),
        (datetime(2024, 10, 1), datetime(2024, 10, 30)),
        (datetime(2024, 11, 1), datetime(2024, 11, 30)),
        (datetime(2024, 12, 1), datetime(2024, 12, 30))
    ]

    for start_date, end_date in date_ranges:
        start_timestamp = int(start_date.timestamp())
        end_timestamp = int(end_date.timestamp())

        match_list_api_url = (
            f"https://europe.api.riotgames.com/lol/match/v5/matches/by-puuid/"
            f"{player_unique_id}/ids?startTime={start_timestamp}&endTime={end_timestamp}&start=0&count=100"
            f"&api_key={api_key}"
        )

        resp = requests.get(match_list_api_url)
        if resp.status_code != 200:
            print(f"Failed to fetch match list for range {start_date} - {end_date}, skipping...")
            continue

        match_ids = resp.json()

        with open(csv_file_path, mode='a', newline='', encoding='utf-8') as file:
            writer = csv.writer(file)

            for match_id in match_ids:
                match_api_url = f"https://europe.api.riotgames.com/lol/match/v5/matches/{match_id}?api_key={api_key}"
                resp = requests.get(match_api_url)

                if resp.status_code != 200:
                    print(f"Failed to fetch data for match {match_id}, skipping...")
                    continue

                match_data = resp.json()
                game_duration = match_data['info']['gameDuration'] / 60

                participants = match_data['metadata']['participants']
                try:
                    player_index = participants.index(player_unique_id)
                except ValueError:
                    print(f"Player {player_unique_id} not found in match {match_id}, skipping...")
                    continue

                player_data = match_data['info']['participants'][player_index]
                date = timestamp_to_date(match_data['info']['gameStartTimestamp'])
                name = player_data.get('championName', 'Unknown')
                position = player_data.get('teamPosition', 'Unknown')
                k = player_data.get('kills', 0)
                d = player_data.get('deaths', 0)
                a = player_data.get('assists', 0)
                win = player_data.get('win', False)
                damage = player_data.get('totalDamageDealtToChampions', 0)
                wizja = player_data.get('visionScore', 0)
                zloto_per_minute = round(player_data.get('goldEarned', 0) / game_duration, 3) if game_duration > 0 else 0
                kda = round((k + a) / d, 3) if d > 0 else "Perfect KDA"
                cs_per_minute = round(player_data.get('totalMinionsKilled', 0) / game_duration, 3) if game_duration > 0 else 0

                kp_raw = player_data.get('challenges', {}).get('killParticipation', math.nan)
                kp = round(kp_raw, 3) if isinstance(kp_raw, (int, float)) else math.nan

                damageprocent_raw = player_data.get('challenges', {}).get('teamDamagePercentage', math.nan)
                damageprocent = round(damageprocent_raw, 3) if isinstance(damageprocent_raw, (int, float)) else math.nan

                flash_raw = player_data.get('challenges', {}).get('multikillsAfterAggressiveFlash', math.nan)
                flash = round(flash_raw, 3) if isinstance(flash_raw, (int, float)) else math.nan

                row = [
                    date, name, position, k, d, a, kda, win,
                    damage, wizja, zloto_per_minute, cs_per_minute,
                    kp, damageprocent, flash
                ]

                for stat in stat_names:
                    value = player_data.get('challenges', {}).get(stat, math.nan)
                    if isinstance(value, (int, float)):
                        value = round(value, 3)
                    else:
                        value = math.nan
                    row.append(value)

                for stat in HEADERS:
                    value = player_data.get(stat)
                    if isinstance(value, (int, float)):
                        value = round(value, 3)
                    row.append(value)

                writer.writerow(row)
        print(f"Processed matches for date range {start_date} to {end_date}. Waiting 2 minutes before continuing...")
        time.sleep(120)
    print(f"Dane zostały zapisane do pliku '{csv_file_path}'.")

if __name__ == '__main__':
    main()
