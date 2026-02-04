# Central CI/CD

Este repo ahora actua como pipeline central (build + integracion + deploy).
Antes, este repo solo enviaba un repository_dispatch a un repo externo
(`bsm-cibot/sm-ci-pipeline`). Ese esquema no servia para este objetivo
porque agregaba un repositorio intermedio con secretos duplicados,
mas mantenimiento y menos trazabilidad del tag/version.

## Cambios aplicados
- Se agrego `/.github/workflows/central-ci.yml` como pipeline central.
- `/.github/workflows/notify-central-ci.yml` quedo desactivado
  (solo `workflow_dispatch` y `if: false`) para evitar loops.

## Flujo actual
1) Repo A corre tests.
2) Repo A dispara `repository_dispatch` a este repo con:
   `source_repo`, `branch`, `sha`, `version`.
3) Este repo hace checkout de Repo A/Repo B segun reglas, build a ACR,
   corre integracion y luego deploy.

## Flujo end-to-end (Repo A -> Central -> Deploy)
1) Repo A ejecuta tests. Si pasan, manda `repository_dispatch` al repo central
   con el payload (`source_repo`, `branch`, `sha`, `version`).
2) El repo central recibe el evento `from-repo-ci`. El payload no trae codigo:
   el runner clona Repo A y Repo B via `actions/checkout` usando
   `PIPELINE_REPO_TOKEN` y el `ref` calculado (SHA/branch o default).
3) Build & push: se construyen las imagenes desde `./repoA` y `./repoB`
   y se publican en ACR con el tag (`version` o short SHA).
4) Integracion: se hace pull de las imagenes desde ACR y se ejecutan
   pruebas E2E/compose (placeholder hoy).
5) Deploy: login a Azure y despliegue al target real (placeholder hoy).

## Ejecucion manual
Se puede disparar `workflow_dispatch` en el repo central y pasar inputs
`source_repo`, `source_branch`, `source_sha`, `version` para simular el
trigger desde Repo A.

## Tag/version
- Si viene `client_payload.version`, se usa ese valor como tag.
- Si no viene, se usa short SHA.

## Ajustes esperados
- Editar `REPO_A`, `REPO_B`, `REPO_A_DEFAULT_REF`, `REPO_B_DEFAULT_REF`
  en `/.github/workflows/central-ci.yml`.
- Ajustar `context`/`Dockerfile` de los builds si la estructura cambia.

## Secrets requeridos en este repo
- `PIPELINE_REPO_TOKEN` (checkout cross-repo)
- `ACR_LOGIN_SERVER`, `ACR_USERNAME`, `ACR_PASSWORD`
- `AZURE_CLIENT_ID`, `AZURE_TENANT_ID`, `AZURE_CLIENT_SECRET`

## Nota sobre otros workflows
`/.github/workflows/docker-publish.yml` queda intacto. Si genera
builds duplicados, conviene desactivarlo o limitar su trigger.
