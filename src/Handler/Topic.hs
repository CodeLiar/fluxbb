{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Topic where

import           Import

import           Data.Time.LocalTime
import           Database.Esqueleto

import           Flux.Forum
import           Flux.Post
import           Flux.Topic

data PostForm = PostForm
  { postFormContent :: Textarea
  } deriving (Show)

postForm :: Form PostForm
postForm =
  renderDivs $ PostForm <$> areq textareaField "Reply Discussion" Nothing

getTopicR :: Int64 -> Handler Html
getTopicR tid = do
  redirect $ TopicPageR tid 1

postTopicR :: Int64 -> Handler Html
postTopicR tid = do
  (uid, name, group) <- allowedToPost
  ((res, wid), enct) <- runFormPost postForm
  case res of
    FormSuccess x -> do
      let content = unTextarea . postFormContent $ x
      (_, page, num) <- replyTopicByPosting uid name (toSqlKey tid) content
      redirect $ TopicPageR tid page :#: ("post-" <> show num)
    _ -> defaultLayout [whamlet|Please.|]

canEdit uid puid group =
  uid == puid || group == Administrator || group == Moderator

getTopicPageR :: Int64 -> Int64 -> Handler Html
getTopicPageR tid page = do
  (uid, name, group) <- allowedToPost
  topic <- getTopicById $ toSqlKey tid
  posts <- getPostsInTopic (toSqlKey tid) page
  forum <- getForumsInformation . topicsForumId . entityVal $ topic
  (wid, enct) <- generateFormPost postForm
  defaultLayout $(widgetFile "topic")

