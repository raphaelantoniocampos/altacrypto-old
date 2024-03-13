import time
import datetime


class DateTimeUtils:
    @staticmethod
    def get_datetime(timestamp=None):
        """
        Get the date and time from the provided timestamp.

        Args:
            timestamp (int, optional): Timestamp to be converted. If not provided, the current timestamp will be used.

        Returns:
            datetime: A datetime object representing the current date and time.
        """
        if not timestamp:
            timestamp = DateTimeUtils.get_current_timestamp()
        return datetime.fromtimestamp(timestamp)

    @staticmethod
    def get_current_timestamp():
        """
        Get the current timestamp.

        Returns:
            int: Current timestamp.
        """
        return int(time.time())

    @staticmethod
    def timedelta_to_string(delta):
        """
        Convert a timedelta object to a string in HH:MM:SS format.

        Args:
            delta (timedelta): The timedelta object to be converted.

        Returns:
            str: String in HH:MM:SS format representing the time specified by delta.
        """
        hours, remainder = divmod(delta.seconds, 3600)
        minutes, seconds = divmod(remainder, 60)
        return f"{hours:02d}:{minutes:02d}:{seconds:02d}"
