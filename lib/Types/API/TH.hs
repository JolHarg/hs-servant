{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Unsafe                #-}

module Types.API.TH where

import Data.Model
import Data.Text
import Language.Haskell.TH
import Servant.API


defineGetPluralType ∷ Model → DecsQ
defineGetPluralType Model { modelName, pluralModelName } = do
    let modelType   = mkName modelName
    let getPluralAPI   = mkName $ "Get" <> pluralModelName <> "API"
    pure [
        TySynD getPluralAPI [] (
            AppT (
                AppT (
                    ConT ''Get
                ) (
                    AppT (
                        AppT PromotedConsT (
                            ConT ''JSON
                        )
                    )
                    PromotedNilT
                )
            ) (
                AppT ListT (
                    ConT modelType
                )
            )
        )
        ]


defineGetType ∷ Model → DecsQ
defineGetType Model { modelName } = do
    let modelType   = mkName modelName
    let getAPI      = mkName $ "Get" <> modelName <> "API"
    pure [
        TySynD getAPI [] (
            AppT (
                AppT (
                    ConT ''Get
                ) (
                    AppT (
                        AppT PromotedConsT (
                            ConT ''JSON
                        )
                    )
                    PromotedNilT
                )
            ) (
                ConT modelType
            )
        )
        ]

defineGetIdType ∷ Model → DecsQ
defineGetIdType Model { modelName } = do
    let modelType   = mkName modelName
    let modelId     = mkName $ modelName <> "Id"
    let getIdAPI      = mkName $ "Get" <> modelName <> "IdAPI"
    pure [
        TySynD getIdAPI [] (
            AppT (
                AppT (
                    ConT ''(:>)
                ) (
                    AppT (
                        AppT (
                            ConT ''Capture
                        ) (
                            LitT (
                                StrTyLit "id"
                            )
                        )
                    ) (
                        ConT modelId
                    )
                )
            ) (
                AppT (
                    AppT (
                        ConT ''Get
                    ) (
                        AppT (
                            AppT PromotedConsT (
                                ConT ''JSON
                            )
                        ) PromotedNilT
                    )
                ) (
                    ConT modelType
                )
            )
        )
        ]

defineDeleteType ∷ Model → DecsQ
defineDeleteType Model { modelName } = do
    let deleteAPI   = mkName $ "Delete" <> modelName <> "API"
    pure [
        -- https://github.com/haskell-servant/servant-auth/issues/177
        TySynD deleteAPI [] (
            AppT (
                AppT (
                    ConT ''Delete
                ) (
                    AppT (
                        AppT PromotedConsT (
                            ConT ''JSON
                        )
                    ) PromotedNilT
                )
            ) (
                ConT ''Text
            )
        )
        ]

defineDeleteIdType ∷ Model → DecsQ
defineDeleteIdType Model { modelName } = do
    let modelId     = mkName $ "Delete" <> modelName <> "Id"
    let deleteIdAPI   = mkName $ "Delete" <> modelName <> "IdAPI"
    pure [
        -- https://github.com/haskell-servant/servant-auth/issues/177
        TySynD deleteIdAPI [] (
            AppT (
                AppT (
                    ConT ''(:>)
                ) (
                    AppT (
                        AppT (
                            ConT ''Capture
                        ) (
                            LitT (
                                StrTyLit "id"
                            )
                        )
                    ) (
                        ConT modelId
                    )
                )
            ) (
                AppT (
                    AppT (
                        ConT ''Delete
                    ) (
                        AppT (
                            AppT PromotedConsT (
                                ConT ''JSON
                            )
                        ) PromotedNilT
                    )
                ) (
                    ConT ''Text
                )
            )
        )
        ]

definePutType ∷ Model → DecsQ
definePutType Model { modelName } = do
    let retrieveModelType   = mkName modelName
    let updateModelType   = mkName ("Update" <> modelName)
    let putAPI      = mkName $ "Put" <> modelName <> "API"
    pure [
        TySynD putAPI [] (
            AppT (
                AppT (
                    ConT ''(:>)
                ) (
                    AppT (
                        AppT (
                            ConT ''ReqBody
                        ) (
                            AppT (
                                AppT PromotedConsT (
                                    ConT ''JSON
                                )
                            ) PromotedNilT
                        )
                    ) (
                        ConT updateModelType
                    )
                )
            ) (
                AppT (
                    AppT (
                        ConT ''Put
                    ) (
                        AppT (
                            AppT PromotedConsT (
                                ConT ''JSON
                            )
                        ) PromotedNilT
                    )
                ) (
                    ConT retrieveModelType
                )
            )
        )
        ]

definePutIdType ∷ Model → DecsQ
definePutIdType Model { modelName } = do
    let retrieveModelType   = mkName modelName
    let updateModelType   = mkName ("Update" <> modelName)
    let modelId     = mkName $ "Update" <> modelName <> "Id"
    let putIdAPI      = mkName $ "Put" <> modelName <> "IdAPI"
    pure [
        TySynD putIdAPI [] (
            AppT (
                AppT (
                    ConT ''(:>)
                ) (
                    AppT (
                        AppT (
                            ConT ''Capture
                        ) (
                            LitT (
                                StrTyLit "id"
                            )
                        )
                    ) (
                        ConT modelId
                    )
                )
            ) (
                AppT (
                    AppT (
                        ConT ''(:>)
                    ) (
                        AppT (
                            AppT (
                                ConT ''ReqBody
                            ) (
                                AppT (
                                    AppT PromotedConsT (
                                        ConT ''JSON
                                    )
                                ) PromotedNilT
                            )
                        ) (
                            ConT updateModelType
                        )
                    )
                ) (
                    AppT (
                        AppT (
                            ConT ''Put
                        ) (
                            AppT (
                                AppT PromotedConsT (
                                    ConT ''JSON
                                )
                            ) PromotedNilT
                        )
                    ) (
                        ConT retrieveModelType
                    )
                )
            )
        )
        ]

definePostType ∷ Model → DecsQ
definePostType Model { modelName } = do
    let createModelType   = mkName ("Create" <> modelName)
    let retrieveModelType   = mkName modelName
    let postAPI     = mkName $ "Post" <> modelName <> "API"
    pure [
        TySynD postAPI [] (
            AppT (
                AppT (
                    ConT ''(:>)
                ) (
                    AppT (
                        AppT (
                            ConT ''ReqBody
                        ) (
                            AppT (
                                AppT PromotedConsT (
                                    ConT ''JSON
                                )
                            ) PromotedNilT
                        )
                    ) (
                        ConT createModelType
                    )
                )
            ) (
                AppT (
                    AppT (
                        ConT ''PostCreated
                    ) (
                        AppT (
                            AppT PromotedConsT (
                                ConT ''JSON
                            )
                        ) PromotedNilT
                    )
                ) (
                    ConT retrieveModelType
                )
            )
        )
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
    let getPlural    = mkName $ "get" <> pluralModelName
    let getPluralAPI   = mkName $ "Get" <> pluralModelName <> "API"
    let getId          = mkName $ "get" <> modelName
    let getIdAPI      = mkName $ "Get" <> modelName <> "IdAPI"
    let deleteId = mkName $ "delete" <> modelName <> "Id"
    let deleteIdAPI   = mkName $ "Delete" <> modelName <> "IdAPI"
    let putId      = mkName $ "put" <> modelName <> "Id"
    let putIdAPI      = mkName $ "Put" <> modelName <> "IdAPI"
    let post     = mkName $ "post" <> modelName
    let postAPI     = mkName $ "Post" <> modelName <> "API"
    let namedAPI = mkName $ pluralModelName <> "NamedAPI"
    let api         = mkName $ pluralModelName <> "API"
    let mode = mkName "mode"
    crud <- defineRESTCRUDTypes model
    pure $ crud <> [
        DataD [] namedAPI [
            PlainTV mode BndrReq
            ] Nothing [
                RecC namedAPI [
                    (
                        getPlural,
                        Bang NoSourceUnpackedness NoSourceStrictness,
                        AppT (
                            AppT (
                                ConT ''(:-)
                            ) (
                                VarT mode
                            )
                        ) (
                            ConT getPluralAPI
                        )
                    ),
                    (
                        getId,
                        Bang NoSourceUnpackedness NoSourceStrictness,
                        AppT (
                            AppT (
                                ConT ''(:-)
                            ) (
                                VarT mode
                            )
                        ) (
                            ConT getIdAPI
                        )
                    ),
                    (
                        deleteId,
                        Bang NoSourceUnpackedness NoSourceStrictness,
                        AppT (
                            AppT (
                                ConT ''(:-)
                            ) (
                                VarT mode
                            )
                        ) (
                            ConT deleteIdAPI
                        )
                    ),
                    (
                        putId,
                        Bang NoSourceUnpackedness NoSourceStrictness,
                        AppT (
                            AppT (
                                ConT ''(:-)
                            ) (
                                VarT mode
                            )
                        ) (
                            ConT putIdAPI
                        )
                    ),
                    (
                        post,
                        Bang NoSourceUnpackedness NoSourceStrictness,
                        AppT (
                            AppT (
                                ConT ''(:-)
                            ) (
                                VarT mode
                            )
                        ) (
                            ConT postAPI
                        )
                    )
                    ]
                ] [],
        TySynD api [] (
            AppT (
                AppT (
                    ConT ''(:>)
                ) (
                    LitT (
                        StrTyLit pluralEndpoint
                    )
                )
            ) (
                AppT (
                    ConT ''NamedRoutes
                ) (
                    ConT namedAPI
                )
            )
        )
        ]

defineRESTTypesSelf ∷ Model → DecsQ
defineRESTTypesSelf model@Model { endpoint, modelName } = do
    let get = mkName $ "get" <> modelName
    let getAPI   = mkName $ "Get" <> modelName <> "API"
    let delete = mkName $ "delete" <> modelName
    let deleteAPI   = mkName $ "Delete" <> modelName <> "API"
    let put = mkName $ "put" <> modelName
    let putAPI      = mkName $ "Put" <> modelName <> "API"
    let namedAPI = mkName $ modelName <> "NamedAPI"
    let api         = mkName $ modelName <> "API"
    let mode = mkName "mode"
    crud <- defineRESTCRUDTypesSelf model
    pure $ crud <> [
        DataD [] namedAPI [
            PlainTV mode BndrReq
            ] Nothing [
                RecC namedAPI [
                    (
                        get,
                        Bang NoSourceUnpackedness NoSourceStrictness,
                        AppT (
                            AppT (
                                ConT ''(:-)
                            ) (
                                VarT mode
                            )
                        ) (
                            ConT getAPI
                        )
                    ),
                    (
                        delete,
                        Bang NoSourceUnpackedness NoSourceStrictness,
                        AppT (
                            AppT (
                                ConT ''(:-)
                            ) (
                                VarT mode
                            )
                        ) (
                            ConT deleteAPI
                        )
                    ),
                    (
                        put,
                        Bang NoSourceUnpackedness NoSourceStrictness,
                        AppT (
                            AppT (
                                ConT ''(:-)
                            ) (
                                VarT mode
                            )
                        ) (
                            ConT putAPI
                        )
                    )
                    ]
                ] [],
        TySynD api [] (
            AppT (
                AppT (
                    ConT ''(:>)
                ) (
                    LitT (
                        StrTyLit endpoint
                    )
                )
            ) (
                AppT (
                    ConT ''NamedRoutes
                ) (
                    ConT namedAPI
                )
            )
        )
        ]

defineReadRESTTypes ∷ Model → DecsQ
defineReadRESTTypes model@Model { pluralEndpoint, modelName, pluralModelName } = do
    let getPlural = mkName $ "get" <> pluralModelName
    let getPluralAPI   = mkName $ "Get" <> pluralModelName <> "API"
    let getId = mkName $ "get" <> modelName
    let getIdAPI      = mkName $ "Get" <> modelName <> "IdAPI"
    let api         = mkName $ "Public" <> pluralModelName <> "API"
    let namedAPI = mkName $ "Public" <> pluralModelName <> "NamedAPI"
    let mode = mkName "mode"
    getType <- defineGetPluralType model
    getIdType <- defineGetIdType model
    pure $ getType <> getIdType <> [
        DataD [] namedAPI [
            PlainTV mode BndrReq
            ] Nothing [
                RecC namedAPI [
                    (
                        getPlural,
                        Bang NoSourceUnpackedness NoSourceStrictness,
                        AppT (
                            AppT (
                                ConT ''(:-)
                            ) (
                                VarT mode
                            )
                        ) (
                            ConT getPluralAPI
                        )
                    ),
                    (
                        getId,
                        Bang NoSourceUnpackedness NoSourceStrictness,
                        AppT (
                            AppT (
                                ConT ''(:-)
                            ) (
                                VarT mode
                            )
                        ) (
                            ConT getIdAPI
                        )
                    )
                    ]
                ] [],
        TySynD api [] (
            AppT (
                AppT (
                    ConT ''(:>)
                ) (
                    LitT (
                        StrTyLit pluralEndpoint
                    )
                )
            ) (
                AppT (
                    ConT ''NamedRoutes
                ) (
                    ConT namedAPI
                )
            )
        )
        ]


