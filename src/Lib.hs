{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( allComments
    ) where

import Text.HTML.Scalpel
import Control.Applicative

type Author = String

data Comment
    = TextComment Author String
    | ImageComment Author URL
    deriving (Show, Eq)

allComments :: IO (Maybe [Comment])
allComments = scrapeURL "https://www.merriam-webster.com/word-of-the-day" comments
   where
       comments :: Scraper String [Comment]
       comments = chroots ("div" @: [hasClass "container"]) comment

       comment :: Scraper String Comment
       comment = textComment <|> imageComment

       textComment :: Scraper String Comment
       textComment = do
           author      <- text $ "span" @: [hasClass "author"]
           commentText <- text $ "div"  @: [hasClass "text"]
           return $ TextComment author commentText

       imageComment :: Scraper String Comment
       imageComment = do
           author   <- text       $ "span" @: [hasClass "author"]
           imageURL <- attr "src" $ "img"  @: [hasClass "image"]
           return $ ImageComment author imageURL
