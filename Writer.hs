{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Writer where

import Data.ByteString.Char8

import Text.XML.Pugi (Document, MutableNode, NodeKind (..))
import Text.XML.Pugi.Mutable hiding (path)

import Types

writeXML :: Graph -> IO Document
writeXML g = create $ \doc -> do
    decl <- appendDeclaration "xml" doc
    appendAttrs [("version", "1.0"), ("encoding", "ASCII")] decl

    graph <- appendElement "Graphs" doc
    appendAttr "xmlns" (pack $ xmlns g) graph
    getGraphTypeElement (graphType g) graph
    getGraphInstanceElement (graphInstance g) graph

getGraphTypeElement :: GraphType -> MutableNode (Element) -> Modify ()
getGraphTypeElement g graph = do
    gt <- appendElement "GraphType" graph
    appendAttr "id" (pack $ graphTypeID g) gt
    mts <- appendElement "MessageTypes" gt
    getMessageTypesElements (messageTypes g) mts
    dts <- appendElement "DeviceTypes" gt
    getDeviceTypesElements (deviceTypes g) dts

getMessageTypesElements :: [MessageType] -> MutableNode (Element) -> Modify ()
getMessageTypesElements [] _ = return ()
getMessageTypesElements (m:ms) mts = do
    mt <- appendElement "MessageType" mts
    appendAttr "id" (pack $ messageID m) mt
    getMessageElements (message m) mt
    getMessageTypesElements ms mts

getMessageElements :: Message -> MutableNode (Element) -> Modify ()
getMessageElements (Message [] []) _ = return ()
getMessageElements (Message x []) mt = do
    m <- appendElement "Message" mt
    s <- appendElement "Scalar" m
    appendAttr "name" (pack x) s
getMessageElements (Message [] x) mt = do
    m <- appendElement "Message" mt
    s <- appendElement "Scalar" m
    appendAttr "type" (pack x) s
getMessageElements (Message x y) mt = do
    m <- appendElement "Message" mt
    s <- appendElement "Scalar" m
    appendAttrs [("name", pack x), ("type", pack y)] s

getDeviceTypesElements :: [DeviceType] -> MutableNode (Element) -> Modify ()
getDeviceTypesElements [] _ = return ()
getDeviceTypesElements (d:ds) dts = do
    dt <- appendElement "DeviceType" dts
    appendAttr "id" (pack $ deviceID d) dt
    getStateElements (states d) dt
    getInputPinElements (inputPins d) dt
    getOutputPinElements (outputPins d) dt
    rts <- appendElement "ReadyToSend" dt
    appendCData (pack $ readyToSend d) rts
    getDeviceTypesElements ds dts

getStateElements :: [State] -> MutableNode (Element) -> Modify ()
getStateElements [] _ = return ()
getStateElements (s:sts) dt = do
    state <- appendElement "State" dt
    scalar <- appendElement "Scalar" state
    appendAttrs [("name", (pack $ stateName s)),
                 ("type", (pack $ stateType s))] scalar
    getStateElements sts dt

getInputPinElements :: [InputPin] -> MutableNode (Element) -> Modify ()
getInputPinElements [] _ = return ()
getInputPinElements (i:is) dt = do
    pin <- appendElement "InputPin" dt
    appendAttrs [("name", (pack $ inputName i)),
                 ("messageTypeId", (pack $ iMessageTypeID i))] pin
    rec <- appendElement "OnReceive" pin
    appendCData (pack $ onReceive i) rec
    getInputPinElements is dt

getOutputPinElements :: [OutputPin] -> MutableNode (Element) -> Modify ()
getOutputPinElements [] _ = return ()
getOutputPinElements (o:os) dt = do
    pin <- appendElement "OutputPin" dt
    appendAttrs [("name", (pack $ outputName o)),
                 ("messageTypeId", (pack $ oMessageTypeID o))] pin
    send <- appendElement "OnSend" pin
    appendCData (pack $ onSend o) send
    getOutputPinElements os dt

getGraphInstanceElement :: GraphInstance -> MutableNode (Element) -> Modify ()
getGraphInstanceElement g graph = do
    gi <- appendElement "GraphInstance" graph
    appendAttrs [("graphTypeId", pack $ instanceGraphTypeID g),
                 ("id", pack $ instanceID g)] gi
    di <- appendElement "DeviceInstances" gi
    getDeviceInstanceElements (deviceInstances g) di
    ei <- appendElement "EdgeInstances" gi
    getEdgeInstanceElements (edgeInstances g) ei

getDeviceInstanceElements :: [DeviceInstance] -> MutableNode (Element) -> Modify ()
getDeviceInstanceElements [] _ = return ()
getDeviceInstanceElements (d:ds) gi = do
    di <- appendElement "DevI" gi
    appendAttrs [("type", pack $ deviceType d),
                 ("id", pack $ deviceInstanceID d)] di
    getDeviceInstanceElements ds gi

getEdgeInstanceElements :: [EdgeInstance] -> MutableNode (Element) -> Modify ()
getEdgeInstanceElements [] _ = return ()
getEdgeInstanceElements (e:es) gi = do
    ei <- appendElement "EdgeI" gi
    appendAttr "path" (pack $ path e) ei
    getEdgeInstanceElements es gi
