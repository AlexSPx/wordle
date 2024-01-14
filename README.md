# Wordle

### Проект за курса по функционално програмиране във ФМИ

## Описание

Реализация на играта Wordle чрез Haskell. За осъществяването й се използва
основно манипулиране и обхождане на списъци и работа с нечисти функции.

Използвано е Cabal - https://www.haskell.org/cabal/

## Структура на проекта

`app/Main.hs` - Понеже използвам емоджита за цветовете е нужно да се конфигурира това, чрез библиотеката `Win32` (която работи само на windows).
Тази част е взета от https://discourse.elm-lang.org/t/help-improve-unicode-support-on-windows/3366/3. Останалото е самото стартиране на програмата, избиране на дължина на думата и режим на играта.

`src/WordUtils.hs` - Тук се съдържат помощни финкции, които се използват навсякъде в кода.

- `loadWords` - Функция със страничен ефект, която се използва за зареждане на димите от текстож файл в паметта на програмата. И заедно с `filterWords` и `randomElement`(още една функция със страничен ефект) се осъществава инизиализацията на играта.
- `GuessType` - Enum с цветовете с които се оценяват думите.
- `evalWord` - Една от най-основните функции за реализирането на играта, приема 2 думи - думата която се опитва да познае играча и произволна дума. Връща оценката на думата, побуквено (списък от `GuessType`)

`app/RegularMode.hs` - Най-обикновената версия на wordle.

- `regularGame` - Представлява един ход на играта. Всеки път проверява дали играча има оставащи опити и ако има оценява думата. Съответно изписва на стандартен изход дали оценката на думата, дали е загибил или спечелил.
  `app/EasyMode.hs` - Лесна версия на играта, където играяът бива предупреждаван ако си противоречи с информацията която знае до сега или дъмата не е в речника.

- `easyMode` - Същата идея като `regularGame`. Единствената разлика е, че пазим информация за миналите ходове на играча под формата на `Map GuessType [(Char, Int)]`, където `(Char, Int)` е кортеж от буква и индекс на срещането. `updateMap` обновява този `Map` след всеки ход.
- `checkWord` - приема `Map GuessType [(Char, Int)]` и нововъвдената дума и проверява побуквено дали не си противоречи играчът спрямо миналите му ходове. Връща `Map` от съсщият тип, където:
  - Gray - знаем, че тези букви не се срещат в димата
  - Yellow - букви за които знаем, че са в думата но не са използвани тук
  - Green - зелени букви, за които знаем, че не са на съответната им позиция
- `showWarings` - приема резултат от `checkWord` и принтира на екрана предупрежденията.

`app/HardMode.hs` - Трудна версия на играта, където програмата може да излъже играча 1 път.

- `hardMode` - Пак аналогично на `easyMode`, в допълнение пазим и дали програмата вече е излъгала играча. Кога ще излъже се определя на случаен принцип чрез `randomRIO (0, 1) :: IO Int`.
- `makeLie` - Приема миналите ходове на играта, както и нововъведената дума от играча. Минава побуквено през думата и определя за всяка буква кои са възможните опции за цвят(списък от `GuessType`), така че да не си противоречи с информацията която се знае до момента. Съответно на случаен принцип, за всяка буква, избира един цвят от списъка.

`app/HelperMode.hs` - Програмата се опитва да познае предварително зададена неизвестна дума от речника

- `helperMode` - аналогично на `regularMode`. Разликата е, че се приемат всички възможни до момента думи(което в началото са всички думи с избраната дължина) и съобщава на потребителя ако има несъответствуе между отговорите му(когато списъкът стане празен).
  `filterWords` - филтрира думите след всеки ход на потребителя, като се грижи, че:
  -- всяка зелена буква си е на мястото
  -- всяка жълта буква се съдържа в думата и е на различна позиция.
  -- всяка сива буква не се съдържа в думата.
  `mostEffectiveWords` - От списък с дими намира тази дума, която елиминира най-много други.

- `data/words.txt` - използван речник https://github.com/first20hours/google-10000-english/blob/master/google-10000-english-no-swears.txt

## Инструкции

Проекта се стартира с `cabal run`

```
Enter word length (1-28)
```

Избира се дължина на думата.

```
Wordle - Haskell project
Gamemodes:
0 - Regular Game
1 - Easy Game
2 - Hard Game
3 - Helper Mode
```

Избира се режим на играта с 0-3

### Regular game (0)

```
Tries left: 6. Enter your guess:
glaze
🟩🟩🟩⬜⬜
Tries left: 5. Enter your guess:
glari
🟩🟩🟩⬜🟨
Tries left: 4. Enter your guess:
glaik
🟩🟩🟩🟩🟩
```

### Easy Game (1)

```
Tries left: 6. Enter your guess:
tries
⬜🟨⬜⬜🟩
"The following chars are not on the correct place[]"
"The following chars are not in the word []"
"You are missing the following yellow chars []"
Tries left: 5. Enter your guess:
paris
⬜🟨🟨⬜🟩
"The following chars are not on the correct place[]"
"The following chars are not in the word [('i',3)]"
"You are missing the following yellow chars []"
Tries left: 4. Enter your guess:
cries
⬜🟨⬜⬜🟩
"The following chars are not on the correct place[]"
"The following chars are not in the word [('e',3),('i',2)]"
"You are missing the following yellow chars [('a',1)]"
Tries left: 3. Enter your guess:
rojas
🟩🟩🟩🟩🟩
```

### Hard Game (2)

`word: oflem`

```
Tries left: 6. Enter your guess:
olafs
🟩🟨⬜🟨⬜
Tries left: 5. Enter your guess:
oplpf
🟩⬜🟩⬜🟩 <- Lied here
Tries left: 4. Enter your guess:
offes
🟩🟩⬜🟩⬜
Tries left: 3. Enter your guess:
oflem
🟩🟩🟩🟩🟩
```

### Helper Game (3)

```
"The word is: sofas"
6 tries left. Enter space-separated list of Colors. (Gray, Yellow, Green)
"My guess is: oates"
Yellow Yellow Gray Gray Green
5 tries left. Enter space-separated list of Colors. (Gray, Yellow, Green)
"My guess is: loans"
Gray Green Yellow Gray Green
4 tries left. Enter space-separated list of Colors. (Gray, Yellow, Green)
"My guess is: sofas"
Green Green Green Green Green
"I win"
```
