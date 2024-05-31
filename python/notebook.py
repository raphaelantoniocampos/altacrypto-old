import random
import string
from core.database_manager import DatabaseManager
from models.user import User, UserSettings


def generate_symbol(le=6) -> str:
    random_symbol = "".join(
        random.choices(string.ascii_uppercase + string.digits, k=le)
    )
    return random_symbol


def generate_users(quantity, usd_balance, logger=False):
    users = []
    names = [
        "Alice",
        "Bob",
        "Charlie",
        "David",
        "Emma",
        "Frank",
        "Grace",
        "Henry",
        "Isabella",
        "Jack",
        "Kate",
        "Liam",
        "Mia",
        "Noah",
        "Olivia",
        "Peter",
        "Quinn",
        "Rachel",
        "Samuel",
        "Tiffany",
        "Uma",
        "Victor",
        "Wendy",
        "Xavier",
        "Yasmine",
        "Zachary",
        "Abigail",
        "Benjamin",
        "Chloe",
        "Daniel",
        "Emily",
        "Fiona",
        "George",
        "Hannah",
        "Ian",
        "Jessica",
        "Kevin",
        "Lily",
        "Matthew",
        "Natalie",
        "Oscar",
        "Penelope",
        "Quentin",
        "Rebecca",
        "Sarah",
        "Thomas",
        "Ursula",
        "Vincent",
        "Wendell",
        "Amanda",
        "Bruno",
        "Carla",
        "Diego",
        "Erica",
        "Fabio",
        "Gabriela",
        "Hugo",
        "Isadora",
        "Joao",
        "Katia",
        "Lucas",
        "Mariana",
        "Natalia",
        "Otavio",
        "Paula",
        "Rafael",
        "Sara",
        "Thiago",
        "Ursula",
        "Vitoria",
        "Wagner",
        "Ximena",
        "Yasmin",
        "Zeca",
        "Adriana",
        "Bernardo",
        "Camila",
        "Davi",
        "Elaine",
        "Fernando",
        "Gisele",
        "Helio",
        "iris",
        "Julio",
        "Karen",
        "Leandro",
        "Marcia",
        "Nathan",
        "Olivia",
        "Patricia",
        "Rodrigo",
        "Selvia",
        "Tulio",
        "Valentina",
        "William",
        "Xuxa",
        "Yago",
        "Zuleide",
    ]
    for i in range(quantity):
        if i == 0 and logger:
            login = "Logger"
            name = "Logger"
            api_key = generate_symbol(20)
            secret_key = generate_symbol(20)
            user_settings = UserSettings()
            str_password = generate_symbol()
            user = User(
                login=login,
                name=name,
                api_key=api_key,
                secret_key=secret_key,
                user_settings=user_settings,
                usd_balance=10000,
                str_password=str_password,
            )
            users.append(user)
            continue

        login = generate_symbol()
        name = random.choice(names)
        api_key = generate_symbol(20)
        secret_key = generate_symbol(20)
        user_settings = UserSettings()
        str_password = generate_symbol()

        user = User(
            login=login,
            name=name,
            api_key=api_key,
            secret_key=secret_key,
            user_settings=user_settings,
            usd_balance=usd_balance,
            str_password=str_password,
        )
        users.append(user)
    return users


def add_users(quantity, usd_balance, logger=False):
    database_manager = DatabaseManager()
    users = generate_users(quantity, usd_balance, logger=logger)
    for user in users:
        database_manager.add_user(user)


current_datetime = datetime.datetime()

# rn = random.randint(0, 5) + 5
# add_users(1, 1000, logger=True)
# print(f"({rn}) users 100 criados")

# add_users(10, 500)
# rn = random.randint(0, 10)
# add_users(1, 100)
# print(f"({10 + rn}) users 500 criados")
#
# add_users(5, 1000)
# rn = random.randint(0, 5)
# add_users(rn, 1000)
# print(f"({5 + rn}) users 1000 criados")

# add_assets(50)
