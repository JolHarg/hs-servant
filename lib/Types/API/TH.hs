{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Unsafe                #-}

module Types.API.TH where

import Data.Model
import Data.Text
import GHC.Generics        (Generic (..))
import Language.Haskell.TH
import Servant.API


defineGetPluralType ∷ Model → DecsQ
defineGetPluralType Model { modelName, pluralModelName } = do
    let modelType   = mkName modelName
    let getPluralAPI   = mkName $ "Get" <> pluralModelName <> "API"
    pure [
        TySynD getPluralAPI [] (`AppT` ConT ''Get (`AppT` (`AppT` PromotedConsT (ConT ''JSON)) PromotedNilT) AppT `AppT` ListT (ConT modelType))
        ]

defineGetType ∷ Model → DecsQ
defineGetType Model { modelName } = do
    let modelType   = mkName modelName
    let getAPI      = mkName $ "Get" <> modelName <> "API"
    pure [
        TySynD getAPI [] (`AppT`
      ConT ''Get
      (`AppT` (`AppT` PromotedConsT (ConT ''JSON)) PromotedNilT)
      `AppT` ConT modelType)
        ]

defineGetIdType ∷ Model → DecsQ
defineGetIdType Model { modelName } = do
    let modelType   = mkName modelName
    let modelId     = mkName $ modelName <> "Id"
    let getIdAPI      = mkName $ "Get" <> modelName <> "IdAPI"
    let litId       = LitT $ StrTyLit "id"
    pure [
        TySynD getIdAPI [] (`AppT`
      ConT ''(:>)
      (`AppT` (`AppT` ConT ''Capture litId) (ConT modelId))
      AppT
        `AppT`
          (`AppT`
             ConT ''Get
             (`AppT` (`AppT` PromotedConsT (ConT ''JSON)) PromotedNilT))
          (ConT modelType))
        ]

defineDeleteType ∷ Model → DecsQ
defineDeleteType Model { modelName } = do
    let deleteAPI   = mkName $ "Delete" <> modelName <> "API"
    pure [
        -- https://github.com/haskell-servant/servant-auth/issues/177
        TySynD deleteAPI [] (`AppT`
      ConT ''Delete
      (`AppT` (`AppT` PromotedConsT (ConT ''JSON)) PromotedNilT)
      `AppT` ConT ''Text)
        ]

defineDeleteIdType ∷ Model → DecsQ
defineDeleteIdType Model { modelName } = do
    let modelId     = mkName $ "Delete" <> modelName <> "Id"
    let deleteIdAPI   = mkName $ "Delete" <> modelName <> "IdAPI"
    let litId       = LitT $ StrTyLit "id"
    pure [
        -- https://github.com/haskell-servant/servant-auth/issues/177
        TySynD deleteIdAPI [] (`AppT`
      ConT ''(:>)
      (`AppT` (`AppT` ConT ''Capture litId) (ConT modelId))
      AppT
        `AppT`
          (`AppT`
             ConT ''Delete
             (`AppT` (`AppT` PromotedConsT (ConT ''JSON)) PromotedNilT))
          (ConT ''Text))
        ]

definePutType ∷ Model → DecsQ
definePutType Model { modelName } = do
    let retrieveModelType   = mkName modelName
    let updateModelType   = mkName ("Update" <> modelName)
    let putAPI      = mkName $ "Put" <> modelName <> "API"
    pure [
        TySynD putAPI [] (`AppT`
      ConT ''(:>)
      (`AppT`
         (`AppT`
            ConT ''ReqBody
            (`AppT` (`AppT` PromotedConsT (ConT ''JSON)) PromotedNilT))
         (ConT updateModelType))
      AppT
        `AppT`
          (`AppT`
             ConT ''Put
             (`AppT` (`AppT` PromotedConsT (ConT ''JSON)) PromotedNilT))
          (ConT retrieveModelType))
        ]

definePutIdType ∷ Model → DecsQ
definePutIdType Model { modelName } = do
    let retrieveModelType   = mkName modelName
    let updateModelType   = mkName ("Update" <> modelName)
    let modelId     = mkName $ "Update" <> modelName <> "Id"
    let putIdAPI      = mkName $ "Put" <> modelName <> "IdAPI"
    let litId       = LitT $ StrTyLit "id"
    pure [
        TySynD putIdAPI [] (`AppT`
      ConT ''(:>)
      (`AppT` (`AppT` ConT ''Capture litId) (ConT modelId))
      AppT
        `AppT`
          (`AppT`
             ConT ''(:>)
             (`AppT`
                (`AppT`
                   ConT ''ReqBody
                   (`AppT` (`AppT` PromotedConsT (ConT ''JSON)) PromotedNilT))
                (ConT updateModelType)))
          (`AppT`
             (`AppT`
                ConT ''Put
                (`AppT` (`AppT` PromotedConsT (ConT ''JSON)) PromotedNilT))
             (ConT retrieveModelType)))
        ]

definePostType ∷ Model → DecsQ
definePostType Model { modelName } = do
    let createModelType   = mkName ("Create" <> modelName)
    let retrieveModelType   = mkName modelName
    let postAPI     = mkName $ "Post" <> modelName <> "API"
    pure [
        TySynD postAPI [] (`AppT`
      ConT ''(:>)
      (`AppT`
         (`AppT`
            ConT ''ReqBody
            (`AppT` (`AppT` PromotedConsT (ConT ''JSON)) PromotedNilT))
         (ConT createModelType))
      AppT
        `AppT`
          (`AppT`
             ConT ''PostCreated
             (`AppT` (`AppT` PromotedConsT (ConT ''JSON)) PromotedNilT))
          (ConT retrieveModelType))
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
defineRESTTypes model@Model { modelName, pluralModelName, endpoint } = do
    let getPlural    = mkName $ "get" <> pluralModelName
    let getPluralAPI   = mkName $ "Get" <> pluralModelName <> "API"
    let getId          = mkName $ "get" <> pluralModelName <> "Id"
    let getIdAPI      = mkName $ "Get" <> modelName <> "IdAPI"
    let deleteId = mkName $ "delete" <> modelName
    let deleteIdAPI   = mkName $ "Delete" <> modelName <> "IdAPI"
    let putId = mkName $ "put" <> modelName <> "Id"
    let putIdAPI      = mkName $ "Put" <> modelName <> "IdAPI"
    let post = mkName $ "post" <> modelName
    let postAPI     = mkName $ "Post" <> modelName <> "API"
    let api         = mkName $ pluralModelName <> "API"
    let namedApi    = mkName $ pluralModelName <> "NamedAPI"
    let mode        = mkName "mode"
    crud <- defineRESTCRUDTypes model
    pure $ crud <> [
      DataD
        []
        namedApi [
          PlainTV
          mode
          BndrReq
        ]
        Nothing
        [
          RecC namedApi [
            (
              getPlural,
              Bang NoSourceUnpackedness NoSourceStrictness,
              AppT (AppT (ConT ''(:-)) (VarT mode)) (ConT getPluralAPI)
            ),
            (
              getId,
              Bang NoSourceUnpackedness NoSourceStrictness,
              AppT (AppT (ConT ''(:-)) (VarT mode)) (ConT getIdAPI)
            ),
            (
              deleteId,
              Bang NoSourceUnpackedness NoSourceStrictness,
              AppT (AppT (ConT ''(:-)) (VarT mode)) (ConT deleteIdAPI)
            ),
            (
              putId,
              Bang NoSourceUnpackedness NoSourceStrictness,
              AppT (AppT (ConT ''(:-)) (VarT mode)) (ConT putIdAPI)
            ),
            (
              post,
              Bang NoSourceUnpackedness NoSourceStrictness,
              AppT (AppT (ConT ''(:-)) (VarT mode)) (ConT postAPI)
            )
          ]
        ]
        [
          DerivClause (Just StockStrategy) [ConT ''Generic]
        ],
      TySynD
        api
        []
        (
          AppT (AppT (ConT ''(:>)) (LitT (StrTyLit endpoint))) (AppT (ConT ''NamedRoutes) (ConT namedApi))
        )
      ]

defineRESTTypesSelf ∷ Model → DecsQ
defineRESTTypesSelf model@Model { modelName, endpoint } = do
    let get = mkName $ "get" <> modelName
    let getAPI   = mkName $ "Get" <> modelName <> "API"
    let delete = mkName $ "delete" <> modelName
    let deleteAPI   = mkName $ "Delete" <> modelName <> "API"
    let put = mkName $ "put" <> modelName
    let putAPI      = mkName $ "Put" <> modelName <> "API"
    let api         = mkName $ modelName <> "API"
    let namedApi    = mkName $ modelName <> "NamedAPI"
    let mode        = mkName "mode"
    crud <- defineRESTCRUDTypesSelf model
    pure $ crud <> [
      DataD
        []
        namedApi [
          PlainTV
          mode
          BndrReq
        ]
        Nothing
        [
          RecC namedApi [
            (
              get,
              Bang NoSourceUnpackedness NoSourceStrictness,
              AppT (AppT (ConT ''(:-)) (VarT mode)) (ConT getAPI)
            ),
            (
              delete,
              Bang NoSourceUnpackedness NoSourceStrictness,
              AppT (AppT (ConT ''(:-)) (VarT mode)) (ConT deleteAPI)
            ),
            (
              put,
              Bang NoSourceUnpackedness NoSourceStrictness,
              AppT (AppT (ConT ''(:-)) (VarT mode)) (ConT putAPI)
            )
          ]
        ]
        [
          DerivClause (Just StockStrategy) [ConT ''Generic]
        ],
      TySynD
        api
        []
        (
          AppT (AppT (ConT ''(:>)) (LitT (StrTyLit endpoint))) (AppT (ConT ''NamedRoutes) (ConT namedApi))
        )
      ]

defineReadRESTTypes ∷ Model → DecsQ
defineReadRESTTypes model@Model { pluralEndpoint, modelName, pluralModelName } = do
    let getPlural = mkName $ "get" <> pluralModelName
    let getPluralAPI   = mkName $ "Get" <> pluralModelName <> "API"
    let getId = mkName $ "get" <> modelName <> "Id"
    let getIdAPI      = mkName $ "Get" <> modelName <> "IdAPI"
    let api         = mkName $ "Public" <> pluralModelName <> "API"
    let namedApi    = mkName $ pluralModelName <> "NamedAPI"
    let mode        = mkName "mode"
    getType <- defineGetPluralType model
    getIdType <- defineGetIdType model
    pure $ getType <> getIdType <> [
    -- TODO redo this part
      DataD
        []
        namedApi [
          PlainTV
          mode
          BndrReq
        ]
        Nothing
        [
          RecC namedApi [
            (
              getPlural,
              Bang NoSourceUnpackedness NoSourceStrictness,
              AppT (AppT (ConT ''(:-)) (VarT mode)) (ConT getPluralAPI)
            ),
            (
              getId,
              Bang NoSourceUnpackedness NoSourceStrictness,
              AppT (AppT (ConT ''(:-)) (VarT mode)) (ConT getIdAPI)
            )
          ]
        ]
        [
          DerivClause (Just StockStrategy) [ConT ''Generic]
        ],
      TySynD
        api
        []
        (
          AppT (AppT (ConT ''(:>)) (LitT (StrTyLit pluralEndpoint))) (AppT (ConT ''NamedRoutes) (ConT namedApi))
        )
      ]
