FROM python:3

WORKDIR /app

COPY app/requirements.txt ./

COPY docker/docker-entrypoint.sh /docker-entrypoint.sh

RUN python3 -m venv .venv

RUN chmod +x /docker-entrypoint.sh

RUN python3 -m venv .venv

RUN pip install --no-cache-dir -r requirements.txt

COPY app/ /app

EXPOSE 5000

ENTRYPOINT ["/docker-entrypoint.sh"]

CMD ["python3", "-m", "main"]
