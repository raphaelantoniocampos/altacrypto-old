import time

# import schedule
from managers.asset_analyzer import AssetAnalyzer

import utils.settings


def main():
    schedule.every(EXECUTION_FREQUENCY_MINUTES).minutes.at(
        ":00").do(AssetAnalyzer.run)
    while True:
        schedule.run_pending()
        time.sleep(1)


if __name__ == "__main__":
    main()
