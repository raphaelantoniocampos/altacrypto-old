import time
import logging
import schedule

from core.asset_analyzer import AssetAnalyzer
from utils.global_settings import GlobalSettings


def main():
    logging.basicConfig(level=logging.INFO)
    logger = logging.getLogger("altacrypto")
    logger.info("Starting bot")

    global_settings = GlobalSettings()
    asset_analyzer = AssetAnalyzer()
    schedule.every(global_settings.EXECUTION_FREQUENCY_MINUTES).minutes.at(":00").do(
        asset_analyzer.run
    )
    while True:
        schedule.run_pending()
        time.sleep(1)


if __name__ == "__main__":
    main()

