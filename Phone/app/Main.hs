module Main where

import Data.Char

data DaPhone = Button {value::Char, text::String} deriving (Eq)

instance Show DaPhone where
  show (Button value text) = concat ["Button " ++ "'" ++ (value : [])++ "'"]

layot :: [DaPhone]
layot = [Button '1' "1",Button '2' "2ABC",Button '3' "3DEF", Button '4' "4GHI", Button '5' "5JKL", Button '6' "6MNO", Button '7' "7PQRS", Button '8' "8TUV", Button '9' "9WXYZ", Button '0' "0+ ", Button '*' "*^", Button '#' "#.,"]


--Выводит нажатия чтобы получить результат
taper :: Char -> [(DaPhone, Int)]
taper value | isUpper value = (pushTimes '^' layot:[]) ++  (pushTimes value layot : [])
            | otherwise = (pushTimes value layot) : []

--Выводит сколько раз надо нажать кнопку чтобы получить значение не учитывает регистр (хэлпер для tapper)
pushTimes :: Char -> [DaPhone]-> (DaPhone, Int)
pushTimes s [] = error "BAD VALUE"
pushTimes s list | exist (toUpper s) (text (head list)) = (,) (head list) (index (toUpper s) (text (head list)))
                 | otherwise = pushTimes s (tail list)

--Функция помошник которая просто выводит индекс элемента в листе (начинает с 1)
index :: Eq a =>  a -> [a] -> Int
index val [] = 0
index val (x:xs) | val == x = 1
                 | otherwise = 1 + index val xs

--Функция - помошник которая выводит присутствует ли элемент в листе
exist :: Eq a => a -> [a] -> Bool
exist val [] = False
exist val (x:xs) | val == x = True
                 | otherwise = exist val xs

--функция - помошник которая выводит массив пар (количество элементов в листе, элемент)
sameAtList :: Eq a =>  [a] -> [(Int,a)]
sameAtList [] = []
sameAtList (list) = (length (filter (== (head list)) list), (head list)) : sameAtList (filter (/= (head list)) list)

--Конвертирует текст формата "..."
textConvertor :: String -> [(DaPhone,Int)]
textConvertor [] = []
textConvertor (x:xs) = taper x ++ textConvertor xs

--Конвертирует текст формата ["...", "..."]
bigTextConvertor :: [String] -> [(DaPhone,Int)]
bigTextConvertor [] = []
bigTextConvertor (x:xs) = textConvertor x ++ bigTextConvertor xs

convo :: [String]
convo = ["Wanna play 20 questions", "Ya", "U 1st haha", "Lol ok. Have u ever tasted alcohol lol", "Lol ya", "Wow ur cool haha. Ur turn", "Ok. Do u think I am pretty Lol", "Lol ya", "Haha thanks just making sure rofl ur turn"]

--Выводит массив количества нажатий на каждую кнопку
fingerTaps :: String -> [(Int,(DaPhone,Int))]
fingerTaps = sameAtList . textConvertor

--Находит найболее часто исполбзованое нажатие
coolestLetter :: String -> [(DaPhone,Int)]
coolestLetter = \text ->  map snd (filter (\(x,y) -> x == (maximum (map fst (fingerTaps text)))) (fingerTaps text))


--Выводит найболее часто встречаемый символ в каждом сообщение формата ["msg","msg"...]
mostPopularLetter :: [String] -> [(DaPhone,Int)]
mostPopularLetter [] = []
mostPopularLetter (x:xs) = coolestLetter x ++ mostPopularLetter xs

--Находит найболее часто использованое слово и переводет его в кнопочный вариант
coolestWord :: String -> [(DaPhone,Int)]
coolestWord list = textConvertor (map snd (filter (\(x,y) -> x == maximum (map fst (sameAtList list) )) (sameAtList list)))



main :: IO ()
main = print ""
