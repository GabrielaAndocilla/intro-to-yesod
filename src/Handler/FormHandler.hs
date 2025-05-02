{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.FormHandler where

import Import
import Data.Text as Text
import Text.Read (readMaybe)
import Yesod.Form.Types
import Yesod.Form.Functions

data Person = Person
    { personName :: Text
    , personAge :: Int
    , personEmail :: Text
    , personPet :: Maybe PetType
    }
    deriving (Show, Read)

data PetType = Cat | Dog | Fish
    deriving (Show, Eq, Enum, Bounded, Read)

-- Applicative form
personAForm :: Form Person
personAForm = renderDivs $ Person
    <$> areq textField "Name" Nothing
    <*> areq intField "Age" Nothing
    <*> areq emailField "Email" Nothing
    <*> aopt (selectFieldList petOptions) "Pet" Nothing
    where
        petOptions :: [(Text, PetType)]
        petOptions = [("Cat" , Cat), ("Dog", Dog), ("Fish", Fish)]


data Course = Course
    { courseName :: Text
    , courseType :: CourseType
    , courseDescription :: Text
    , courseCredits :: Maybe Int
    }
    deriving (Show, Read)

data CourseType = Online | Onsite
    deriving (Show, Eq, Enum, Bounded, Read)

registerToCourseForm :: Html -> MForm Handler (FormResult Course, Widget)
registerToCourseForm extra = do
    (nameRes, nameView) <- mreq textField nameSettings Nothing
    (typeRes, typeView) <- mreq (selectFieldList courseTypeOptions) typeSettings Nothing
    (descRes, descView) <- mreq textField descSettings Nothing

    (creditRes, creditView) <- case typeRes of
        FormSuccess Onsite -> mopt intField creditSettings Nothing
        _ -> do
              let emptyView = FieldView
                    { fvId = "no-credits"
                    , fvLabel = "Credits"
                    , fvTooltip = Nothing
                    , fvInput = return ()
                    , fvErrors = Nothing
                    , fvRequired = False
                    }
              return (FormSuccess Nothing, emptyView)

    let courseResult = case (nameRes, typeRes, descRes) of
            (FormSuccess name, FormSuccess typ, FormSuccess desc) ->
                FormSuccess $ Course name typ desc (case creditRes of
                                                      FormSuccess c -> c
                                                      _ -> Nothing)
            (FormFailure e, _, _) -> FormFailure e
            (_, FormFailure e, _) -> FormFailure e
            (_, _, FormFailure e) -> FormFailure e
            _ -> FormMissing

    let widget = do
            toWidget $ [whamlet|
                ^{extra}
                <div .form-group>
                    <label for=#{fvId nameView}>#{fvLabel nameView}
                    ^{fvInput nameView}
                <div .form-group>
                    <label for=#{fvId typeView}>#{fvLabel typeView}
                    ^{fvInput typeView}
                <div .form-group>
                    <label for=#{fvId descView}>#{fvLabel descView}
                    ^{fvInput descView}
                $case typeRes
                    $of FormSuccess Onsite
                        <div .form-group .credits-field>
                            <label for=#{fvId creditView}>#{fvLabel creditView}
                            ^{fvInput creditView}
                    $of _
                        <div .no-credits>
            |]

    return (courseResult, widget)
    where
        nameSettings = FieldSettings
            { fsLabel = "Name"
            , fsTooltip = Nothing
            , fsId = Just "courseName"
            , fsName = Just "name"
            , fsAttrs = [("class", "form-control")]
            }
        typeSettings = FieldSettings
            { fsLabel = "Type"
            , fsTooltip = Nothing
            , fsId = Just "courseType"
            , fsName = Just "type"
            , fsAttrs = [("class", "form-control")]
            }
        descSettings = FieldSettings
            { fsLabel = "Description"
            , fsTooltip = Nothing
            , fsId = Just "courseDescription"
            , fsName = Just "description"
            , fsAttrs = [("class", "form-control")]
            }
        creditSettings = FieldSettings
            { fsLabel = "Credits"
            , fsTooltip = Nothing
            , fsId = Just "courseCredits"
            , fsName = Just "credits"
            , fsAttrs = [("class", "form-control")]
            }
        courseTypeOptions :: [(Text, CourseType)]
        courseTypeOptions = [("Online", Online), ("Onsite", Onsite)]

readPersonFromSession :: Text -> Maybe Person
readPersonFromSession txt = readMaybe (Text.unpack txt)

readCourseFromSession :: Text -> Maybe Course
readCourseFromSession txt = readMaybe (Text.unpack txt)

getExampleFormsR :: Handler Html
getExampleFormsR = do
    mPersonSession <- lookupSession "personData"
    mCourseSession <- lookupSession "courseData"

    (widgetApplicative, enctypeA) <- generateFormPost personAForm
    (widgetMonad, enctypeM) <- generateFormPost registerToCourseForm

    let mPerson = mPersonSession >>= readPersonFromSession
        mCourse = mCourseSession >>= readCourseFromSession

    defaultLayout $ do
        setTitle "Example Forms"
        $(widgetFile "example-forms")

postExampleFormsApplicativeR :: Handler Html
postExampleFormsApplicativeR = do
    ((personResult, widgetApplicative), enctypeA) <- runFormPost personAForm

    mCourseSession <- lookupSession "courseData"
    let mCourse = mCourseSession >>= readCourseFromSession

    (widgetMonad, enctypeM) <- generateFormPost registerToCourseForm

    case personResult of
        FormSuccess person -> do
            setSession "personData" (Text.pack $ show person)

            defaultLayout $ do
                setTitle "Example Forms"
                let mPerson = Just person
                $(widgetFile "example-forms")

        _ -> defaultLayout $ do
            setTitle "Example Forms"
            let mPerson = Nothing :: Maybe Person
            $(widgetFile "example-forms")

postExampleFormsMonadR :: Handler Html
postExampleFormsMonadR = do
    ((courseResult, widgetMonad), enctypeM) <- runFormPost registerToCourseForm

    mPersonSession <- lookupSession "personData"
    let mPerson = mPersonSession >>= readPersonFromSession

    (widgetApplicative, enctypeA) <- generateFormPost personAForm

    case courseResult of
        FormSuccess course -> do
            setSession "courseData" (Text.pack $ show course)

            defaultLayout $ do
                setTitle "Example Forms"
                let mCourse = Just course
                $(widgetFile "example-forms")

        _ -> defaultLayout $ do
            setTitle "Example Forms"
            let mCourse = Nothing :: Maybe Course
            $(widgetFile "example-forms")
