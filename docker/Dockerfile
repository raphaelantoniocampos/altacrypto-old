FROM python:3

WORKDIR /app

COPY .app/requirements.txt ./

RUN pip install --no-cache-dir -r requirements.txt

COPY ./app /app

COPY ./docker-entrypoint.sh /docker-entrypoint.sh

ENTRYPOINT /docker-entrypoint.sh $0 $@

CMD ["python", "-m", "main.py"]
