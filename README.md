# rjson

Расширение JSON метками `ident@value` и ссылками `@ident`.
Учебный проект курса «Теория и реализация языков программирования» (МФТИ, 2026).

## Формат

```text
{
  universities: [ mipt@{"name": "MIPT"}, msu@{"name": "MSU"} ],
  "students": [
    {name: "Ivan",  university: @mipt},
    {name: "Peter", university: @msu}
  ]
}
```

## Сборка

```bash
cabal build
cabal run rjson < input.rjson
```

## Команда

- TODO
- TODO
- TODO
