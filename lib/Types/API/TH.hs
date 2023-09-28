{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeOperators         #-}

module Types.API.TH where

import Data.Model
import Data.Text
import Language.Haskell.TH
import Servant.API

(<||>) ∷ Type → Type → Type
(<||>) = AppT

infixl 5 <||>

(<$|$>) ∷ Type → Type → Type
a <$|$> b = ConT ''(:<|>) <||> a <||> b

infixr 5 <$|$>

defineGetPluralType ∷ Model → DecsQ
defineGetPluralType Model { modelName, pluralModelName } = do
    let modelType   = mkName modelName
    let getPluralAPI   = mkName $ "Get" <> pluralModelName <> "API"
    pure [
        TySynD getPluralAPI [] (AppT (AppT (ConT ''Get) (AppT (AppT PromotedConsT (ConT ''JSON)) PromotedNilT)) (AppT ListT (ConT modelType)))
        ]

defineGetType ∷ Model → DecsQ
defineGetType Model { modelName } = do
    let modelType   = mkName modelName
    let getAPI      = mkName $ "Get" <> modelName <> "API"
    pure [
        TySynD getAPI [] (AppT (AppT (ConT ''Get) (AppT (AppT PromotedConsT (ConT ''JSON)) PromotedNilT)) (ConT modelType))
        ]

defineGetIdType ∷ Model → DecsQ
defineGetIdType Model { modelName } = do
    let modelType   = mkName modelName
    let modelId     = mkName $ modelName <> "Id"
    let getIdAPI      = mkName $ "Get" <> modelName <> "IdAPI"
    let litId       = LitT $ StrTyLit "id"
    pure [
        TySynD getIdAPI [] (AppT (AppT (ConT ''(:>)) (AppT (AppT (ConT ''Capture) litId) (ConT modelId))) (AppT (AppT (ConT ''Get) (AppT (AppT PromotedConsT (ConT ''JSON)) PromotedNilT)) (ConT modelType)))
        ]

defineDeleteType ∷ Model → DecsQ
defineDeleteType Model { modelName } = do
    let deleteAPI   = mkName $ "Delete" <> modelName <> "API"
    pure [
        -- https://github.com/haskell-servant/servant-auth/issues/177
        TySynD deleteAPI [] (AppT (AppT (ConT ''Delete) (AppT (AppT PromotedConsT (ConT ''JSON)) PromotedNilT)) (ConT ''Text))
        ]

defineDeleteIdType ∷ Model → DecsQ
defineDeleteIdType Model { modelName } = do
    let modelId     = mkName $ modelName <> "Id"
    let deleteIdAPI   = mkName $ "Delete" <> modelName <> "IdAPI"
    let litId       = LitT $ StrTyLit "id"
    pure [
        -- https://github.com/haskell-servant/servant-auth/issues/177
        TySynD deleteIdAPI [] (AppT (AppT (ConT ''(:>)) (AppT (AppT (ConT ''Capture) litId) (ConT modelId))) (AppT (AppT (ConT ''Delete) (AppT (AppT PromotedConsT (ConT ''JSON)) PromotedNilT)) (ConT ''Text)))
        ]

definePutType ∷ Model → DecsQ
definePutType Model { modelName } = do
    let modelType   = mkName modelName
    let putAPI      = mkName $ "Put" <> modelName <> "API"
    pure [
        TySynD putAPI [] (AppT (AppT (ConT ''(:>)) (AppT (AppT (ConT ''ReqBody) (AppT (AppT PromotedConsT (ConT ''JSON)) PromotedNilT)) (ConT modelType))) (AppT (AppT (ConT ''Put) (AppT (AppT PromotedConsT (ConT ''JSON)) PromotedNilT)) (ConT modelType)))
        ]

definePutIdType ∷ Model → DecsQ
definePutIdType Model { modelName } = do
    let modelType   = mkName modelName
    let modelId     = mkName $ modelName <> "Id"
    let putIdAPI      = mkName $ "Put" <> modelName <> "IdAPI"
    let litId       = LitT $ StrTyLit "id"
    pure [
        TySynD putIdAPI [] (AppT (AppT (ConT ''(:>)) (AppT (AppT (ConT ''Capture) litId) (ConT modelId))) (AppT (AppT (ConT ''(:>)) (AppT (AppT (ConT ''ReqBody) (AppT (AppT PromotedConsT (ConT ''JSON)) PromotedNilT)) (ConT modelType))) (AppT (AppT (ConT ''Put) (AppT (AppT PromotedConsT (ConT ''JSON)) PromotedNilT)) (ConT modelType))))
        ]

definePostType ∷ Model → DecsQ
definePostType Model { modelName } = do
    let modelType   = mkName modelName
    let postAPI     = mkName $ "Post" <> modelName <> "API"
    pure [
        TySynD postAPI [] (AppT (AppT (ConT ''(:>)) (AppT (AppT (ConT ''ReqBody) (AppT (AppT PromotedConsT (ConT ''JSON)) PromotedNilT)) (ConT modelType))) (AppT (AppT (ConT ''PostCreated) (AppT (AppT PromotedConsT (ConT ''JSON)) PromotedNilT)) (ConT modelType)))
        ]

defineRESTCRUDTypes ∷ Model → DecsQ
defineRESTCRUDTypes model = do
    getPluralType <- defineGetPluralType model
    getIdType <- defineGetIdType model
    deleteIdType <- defineDeleteIdType model
    putIdType <- definePutIdType model
    postType <- definePostType model
    pure $ getPluralType <> getIdType <> deleteIdType <> putIdType <> postType

defineRESTCRUDTypesSelf ∷ Model → DecsQ
defineRESTCRUDTypesSelf model = do
    getType <- defineGetType model
    deleteType <- defineDeleteType model
    putType <- definePutType model
    pure $
        getType <>
        deleteType <>
        putType

defineRESTTypes ∷ Model → DecsQ
defineRESTTypes model@Model { pluralEndpoint, modelName, pluralModelName } = do
    let getPluralAPI   = mkName $ "Get" <> pluralModelName <> "API"
    let getIdAPI      = mkName $ "Get" <> modelName <> "IdAPI"
    let deleteIdAPI   = mkName $ "Delete" <> modelName <> "IdAPI"
    let putIdAPI      = mkName $ "Put" <> modelName <> "IdAPI"
    let postAPI     = mkName $ "Post" <> modelName <> "API"
    let api         = mkName $ pluralModelName <> "API"
    crud <- defineRESTCRUDTypes model
    pure $ crud <> [
        TySynD api [] (
            ConT ''(:>) <||> LitT (StrTyLit pluralEndpoint) <||>
            (
                ConT getPluralAPI <$|$>
                ConT getIdAPI <$|$>
                ConT deleteIdAPI <$|$>
                ConT putIdAPI <$|$>
                ConT postAPI
            )
        )
        ]

defineRESTTypesSelf ∷ Model → DecsQ
defineRESTTypesSelf model@Model { endpoint, modelName } = do
    let getAPI   = mkName $ "Get" <> modelName <> "API"
    let deleteAPI   = mkName $ "Delete" <> modelName <> "API"
    let putAPI      = mkName $ "Put" <> modelName <> "API"
    let api         = mkName $ modelName <> "API"
    crud <- defineRESTCRUDTypesSelf model
    pure $ crud <> [
        TySynD api [] (ConT ''(:>) <||> LitT (StrTyLit endpoint) <||> (
            ConT getAPI <$|$>
            ConT deleteAPI <$|$>
            ConT putAPI
            ))
        ]

defineReadRESTTypes ∷ Model → DecsQ
defineReadRESTTypes model@Model { pluralEndpoint, modelName, pluralModelName } = do
    let getPluralAPI   = mkName $ "Get" <> pluralModelName <> "API"
    let getIdAPI      = mkName $ "Get" <> modelName <> "IdAPI"
    let api         = mkName $ "Public" <> pluralModelName <> "API"
    getType <- defineGetPluralType model
    getIdType <- defineGetIdType model
    pure $ getType <> getIdType <> [
        TySynD api [] (
            ConT ''(:>) <||> LitT (StrTyLit pluralEndpoint) <||>
            (
                ConT getPluralAPI <$|$>
                ConT getIdAPI
            )
        )
        ]


