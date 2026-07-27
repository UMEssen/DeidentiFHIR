# Multi-stage Docker build for deidentifhir-service-http.
#
# Stage 1: build the layered jar (thin jar + lib/ sidecar) with Maven.
# Stage 2: copy both into a minimal JRE image and run.
#
# Build:    docker build -f service-http/Dockerfile -t deidentifhir-service .
# Run:      docker run -p 8080:8080 deidentifhir-service
#
# To use the openai/privacy-filter handler, mount a config that references it
# AND a writable cache volume so the model survives container restarts:
#
#   docker run -p 8080:8080 \
#     -v $PWD/myconfig.yaml:/etc/deidentifhir/config.yaml \
#     -v deidentifhir-pii-cache:/var/lib/deidentifhir-pii \
#     -e DEIDENTIFHIR_CONFIG=/etc/deidentifhir/config.yaml \
#     -e DEIDENTIFHIR_PII_CACHE_DIR=/var/lib/deidentifhir-pii \
#     deidentifhir-service
#
# The model is ~2.85 GB. Set `prewarm: true` in pii-openai config and your
# k8s readinessProbe to expect a longer startup, or leave prewarm off and
# accept that the first request hitting redact-pii pays the load cost.

# ---- Stage 1: build ---------------------------------------------------------
FROM maven:3.9-eclipse-temurin-21 AS builder
WORKDIR /src

# Copy poms first so dependency resolution caches across source-only changes.
COPY pom.xml ./
COPY core/pom.xml core/pom.xml
COPY pii-openai/pom.xml pii-openai/pom.xml
COPY service-http/pom.xml service-http/pom.xml

# Pre-fetch dependencies. This step is the expensive one to cache.
RUN mvn -B -pl service-http -am dependency:go-offline -DskipTests || true

# Now copy sources and build. The package phase produces:
#   service-http/target/deidentifhir-service-http-<v>.jar  (thin jar)
#   service-http/target/lib/                                (runtime deps)
COPY core/src core/src
COPY pii-openai/src pii-openai/src
COPY service-http/src service-http/src
RUN mvn -B -pl service-http -am package -DskipTests

# ---- Stage 2: runtime -------------------------------------------------------
FROM eclipse-temurin:21-jre

# Drop privileges. UID 10001 keeps us out of the system-uid range and
# matches common k8s securityContext patterns.
RUN groupadd --system --gid 10001 deidentifhir \
 && useradd  --system --uid 10001 --gid deidentifhir --create-home deidentifhir

WORKDIR /app
# Thin jar's manifest already lists `lib/*` on its classpath — pekko's
# reference.conf stays intact this way (no fragile shading required).
COPY --from=builder /src/service-http/target/deidentifhir-service-http-*.jar /app/service.jar
COPY --from=builder /src/service-http/target/lib /app/lib

USER deidentifhir
ENV JAVA_OPTS=""
EXPOSE 8080
ENTRYPOINT ["sh", "-c", "exec java $JAVA_OPTS -jar /app/service.jar"]
