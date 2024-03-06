import schedule
import time
import csv
import re

import pandas as pd
from datetime import datetime

import utils.settings
from untitled_crypto_bot.managers.asset_analyzer import AssetAnalyzer


def main():
    schedule.every(EXECUTION_FREQUENCY_MINUTES).minutes.at(":00").do(AssetAnalyzer.run)
    while True:
        schedule.run_pending()
        time.sleep(1)

if __name__ == "__main__":
    main()
