import time

import schedule
from managers.asset_analyzer import AssetAnalyzer

import utils.settings as settings


def main():
    AssetAnalyzer.run()
    schedule.every(settings.EXECUTION_FREQUENCY_MINUTES).minutes.at(":00").do(
        AssetAnalyzer.run
    )
    while True:
        schedule.run_pending()
        time.sleep(1)


if __name__ == "__main__":
    main()
