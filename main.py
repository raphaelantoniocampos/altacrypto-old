import time
import schedule

# from core.asset_analyzer import AssetAnalyzer
from utils.global_settings import GlobalSettings
from data_access.database_manager import DatabaseManager


def main():
    global_settings = GlobalSettings()
    global_settings.logger.info("Starting bot")
    """
    asset_analyzer = AssetAnalyzer()
    schedule.every(global_settings.EXECUTION_FREQUENCY_MINUTES).minutes.at(":00").do(
        asset_analyzer.run
    )
    """

    # testing
    database = DatabaseManager()
    collection_name = database.get_collection("crypto_prices")
    cursor = collection_name.find()
    for document in cursor:
        print(document)

    # asset_analyzer.run()  # line for testing

    while False:
        schedule.run_pending()
        time.sleep(1)


if __name__ == "__main__":
    main()
