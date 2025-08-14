{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Unsafe                #-}

module Types.API.TH where

import Data.Model
import Data.Text
import GHC.Generics (Generic)
import Language.Haskell.TH
import Servant.API


defineGetPluralType ∷ Model → DecsQ
defineGetPluralType Model { singularType, pluralType } = do
    let modelType   = mkName singularType
    let getPluralAPI   = mkName $ "Get" <> pluralType <> "API"
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
defineGetType Model { singularType } = do
    let modelType   = mkName singularType
    let getAPI      = mkName $ "Get" <> singularType <> "API"
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
defineGetIdType Model { singularType } = do
    let modelType   = mkName singularType
    let modelId     = mkName $ singularType <> "Id"
    let getIdAPI      = mkName $ "Get" <> singularType <> "IdAPI"
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
defineDeleteType Model { singularType } = do
    let deleteAPI   = mkName $ "Delete" <> singularType <> "API"
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
defineDeleteIdType Model { singularType } = do
    let modelId     = mkName $ "Delete" <> singularType <> "Id"
    let deleteIdAPI   = mkName $ "Delete" <> singularType <> "IdAPI"
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
definePutType Model { singularType } = do
    let retrieveModelType   = mkName singularType
    let updateModelType   = mkName ("Update" <> singularType)
    let putAPI      = mkName $ "Put" <> singularType <> "API"
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
definePutIdType Model { singularType } = do
    let retrieveModelType   = mkName singularType
    let updateModelType   = mkName ("Update" <> singularType)
    let modelId     = mkName $ "Update" <> singularType <> "Id"
    let putIdAPI      = mkName $ "Put" <> singularType <> "IdAPI"
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
definePostType Model { singularType } = do
    let createModelType   = mkName ("Create" <> singularType)
    let retrieveModelType   = mkName singularType
    let postAPI     = mkName $ "Post" <> singularType <> "API"
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
defineRESTTypes model@Model { pluralEndpoint, singularType, pluralType } = do
    let getPlural    = mkName $ "get" <> pluralType
    let getPluralAPI   = mkName $ "Get" <> pluralType <> "API"
    let getId          = mkName $ "get" <> singularType
    let getIdAPI      = mkName $ "Get" <> singularType <> "IdAPI"
    let deleteId = mkName $ "delete" <> singularType <> "Id"
    let deleteIdAPI   = mkName $ "Delete" <> singularType <> "IdAPI"
    let putId      = mkName $ "put" <> singularType <> "Id"
    let putIdAPI      = mkName $ "Put" <> singularType <> "IdAPI"
    let post     = mkName $ "post" <> singularType
    let postAPI     = mkName $ "Post" <> singularType <> "API"
    let namedAPI = mkName $ pluralType <> "NamedAPI"
    let api         = mkName $ pluralType <> "API"
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
                ] [
                    DerivClause (Just StockStrategy) [ConT ''Generic]
                ],
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
defineRESTTypesSelf model@Model { singularEndpoint, singularType } = do
    let get = mkName $ "get" <> singularType
    let getAPI   = mkName $ "Get" <> singularType <> "API"
    let delete = mkName $ "delete" <> singularType
    let deleteAPI   = mkName $ "Delete" <> singularType <> "API"
    let put = mkName $ "put" <> singularType
    let putAPI      = mkName $ "Put" <> singularType <> "API"
    let namedAPI = mkName $ singularType <> "NamedAPI"
    let api         = mkName $ singularType <> "API"
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
                ] [
                    DerivClause (Just StockStrategy) [ConT ''Generic]
                ],
        TySynD api [] (
            AppT (
                AppT (
                    ConT ''(:>)
                ) (
                    LitT (
                        StrTyLit singularEndpoint
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

-- let's discover what we should include so we shouldn't need to decide if it's readresttypes or whatever
defineReadRESTTypes ∷ Model → DecsQ
defineReadRESTTypes model@Model { pluralEndpoint, singularType, pluralType } = do
    let getPlural = mkName $ "get" <> pluralType
    let getPluralAPI   = mkName $ "Get" <> pluralType <> "API"
    let getId = mkName $ "get" <> singularType
    let getIdAPI      = mkName $ "Get" <> singularType <> "IdAPI"
    let api         = mkName $ "Public" <> pluralType <> "API"
    let namedAPI = mkName $ "Public" <> pluralType <> "NamedAPI"
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
                ] [
                    DerivClause (Just StockStrategy) [ConT ''Generic]
                ],
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


