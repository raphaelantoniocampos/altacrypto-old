import time
import schedule

from core.asset_analyzer import AssetAnalyzer
from utils.global_settings import GlobalSettings


def main():
    global_settings = GlobalSettings()
    global_settings.logger.info("Starting bot")
    asset_analyzer = AssetAnalyzer()
    schedule.every(global_settings.EXECUTION_FREQUENCY_MINUTES).minutes.at(":00").do(
        asset_analyzer.run
    )

    asset_analyzer.run()  # line for testing

    while True:
        schedule.run_pending()
        time.sleep(1)


if __name__ == "__main__":
    main()


