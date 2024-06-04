FROM python:3

WORKDIR /app

COPY requirements.txt ./

COPY docker-entrypoint.sh ./

RUN python3 -m venv .venv

RUN . .venv/bin/activate && pip install --no-cache-dir -r requirements.txt

COPY ./src ./

RUN mkdir -p /src/logs

EXPOSE 5000

RUN chmod +x ./docker-entrypoint.sh

ENTRYPOINT ["./docker-entrypoint.sh"]

CMD ["python3", "-m", "main"]
