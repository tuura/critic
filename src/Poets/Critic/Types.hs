module Poets.Critic.Types (
  Graph (..), GraphType (..),
  MessageType (..), Message (..),
  DeviceType (..), Property (..), State (..),
  InputPin (..), OutputPin (..),
  GraphInstance (..), DeviceInstance (..),
  DeviceProperty (..), EdgeInstance (..), getGraphType,
  getGraphInstance, getMessageTypes,
  getDeviceType, getDeviceTypes, findDeviceType,
  getDeviceInstances, findDeviceInstance,
  getEdgeInstances, getDeviceInstancesOfType
  ) where

import Data.List

data Graph = Graph
            {
                xmlns         :: String,
                graphType     :: GraphType,
                graphInstance :: GraphInstance
            } deriving Eq

instance Show Graph where
    show g = "Graph\n" ++
             "    xmlns = " ++ xmlns g ++ "\n" ++
             "    " ++ (show $ graphType g) ++ "\n" ++
             "    " ++ (show $ graphInstance g)

getGraphType :: Graph -> GraphType
getGraphType g = graphType g

getGraphInstance :: Graph -> GraphInstance
getGraphInstance g = graphInstance g

data GraphType = GraphType
               {
                   graphTypeID  :: String,
                   messageTypes :: [MessageType],
                   deviceTypes  :: [DeviceType]
               } deriving Eq

instance Show GraphType where
    show g = "GraphType\n" ++
             "    id = " ++ graphTypeID g ++ "\n" ++
             "        " ++ (show $ messageTypes g) ++ "\n" ++
             "        " ++ (show $ deviceTypes g)

getMessageTypes :: Graph -> [MessageType]
getMessageTypes g = messageTypes $ graphType g

getDeviceTypes :: Graph -> [DeviceType]
getDeviceTypes g = deviceTypes $ graphType g

getDeviceType :: Graph -> String -> DeviceType
getDeviceType g i = head $ filter (\s -> deviceID s == i) dts
  where
    dts = deviceTypes $ graphType g

data MessageType = MessageType
                  {
                      messageID :: String,
                      message   :: Message
                  } deriving Eq

instance Show MessageType where
    show m = "MessageType\n" ++
             "        id = " ++ messageID m ++ "\n" ++
             "            " ++ (show $ message m) ++ "\n"

data Message = Message
             {
                 name        :: String,
                 messageType :: String
             } deriving Eq

instance Show Message where
    show m = "Message\n" ++
             "            name = " ++ name m ++ "\n" ++
             "            type = " ++ messageType m

data DeviceType = DeviceType
                {
                    deviceID     :: String,
                    properties   :: [Property],
                    states       :: [State],
                    inputPins    :: [InputPin],
                    outputPins   :: [OutputPin],
                    readyToSend  :: String
                } deriving Eq

instance Show DeviceType where
    show d = "DeviceType\n" ++
             "            " ++ deviceID d ++ "\n" ++
             "            " ++ (show $ properties d) ++ "\n" ++
             "            " ++ (show $ states d) ++ "\n" ++
             "            " ++ (show $ inputPins d) ++ "\n" ++
             "            " ++ (show $ outputPins d) ++ "\n" ++
             "            ReadyToSend = " ++ readyToSend d ++ "\n"

findDeviceType :: String -> [DeviceType] -> DeviceType
findDeviceType t dts = dt index
  where
    index = elemIndex t typeIDs
    typeIDs = map deviceID dts
    dt (Just i) = dts !! i
    dt (Nothing) = DeviceType ("No device of type \"" ++ t ++ "\"")
                              [] [] [] [] []

data Property = Property
              {
                  propertyType    :: String,
                  propertyName    :: String,
                  propertyDefault :: String
              } deriving Eq

instance Show Property where
  show p = "Property\n" ++
           "          Property type   : " ++ propertyType p ++ "\n" ++
           "          Property name   : " ++ propertyName p ++ "\n" ++
           "          Property default: " ++ propertyDefault p ++ "\n"

data State = State
           {
               stateName :: String,
               stateType :: String
           } deriving Eq

instance Show State where
    show s = "State\n" ++
             "            name = " ++ stateName s ++ "\n" ++
             "            type = " ++ stateType s ++ "\n"

data InputPin = InputPin
              {
                  inputName      :: String,
                  iMessageTypeID :: String,
                  onReceive      :: String
              } deriving Eq

instance Show InputPin where
    show i = "InputPin\n" ++
             "            name = " ++ inputName i ++ "\n" ++
             "            messageTypeID = " ++ iMessageTypeID i ++ "\n" ++
             "            onReceive = " ++ onReceive i ++ "\n"

data OutputPin = OutputPin
               {
                   outputName    :: String,
                   oMessageTypeID :: String,
                   onSend        :: String
               } deriving Eq

instance Show OutputPin where
    show o = "OutputPin\n" ++
             "            name = " ++ outputName o ++ "\n" ++
             "            messageTypeID = " ++ oMessageTypeID o ++ "\n" ++
             "            onSend = " ++ onSend o ++ "\n"

data GraphInstance = GraphInstance
                   {
                       instanceGraphTypeID :: String,
                       instanceID          :: String,
                       deviceInstances     :: [DeviceInstance],
                       edgeInstances       :: [EdgeInstance]
                   } deriving Eq

instance Show GraphInstance where
    show g = "GraphInstance\n" ++
             "    graphTypeID = " ++ instanceGraphTypeID g ++ "\n" ++
             "    instanceID = " ++ instanceID g ++ "\n" ++
             "    " ++ (show $ deviceInstances g) ++ "\n" ++
             "    " ++ (show $ edgeInstances g) ++ "\n"

getDeviceInstances :: Graph -> [DeviceInstance]
getDeviceInstances g = deviceInstances $ getGraphInstance g

findDeviceInstance :: String -> [DeviceInstance] -> DeviceInstance
findDeviceInstance i dis = di index
  where
    index = elemIndex i instanceIDs
    instanceIDs = map deviceInstanceID dis
    di (Just x) = dis !! x
    di (Nothing) = DeviceInstance ("No instance of ID \"" ++ i ++ "\"") [] []

data DeviceInstance = DeviceInstance
                    {
                        deviceType         :: String,
                        deviceInstanceID   :: String,
                        deviceProperties   :: [DeviceProperty]
                    } deriving Eq

instance Show DeviceInstance where
    show d = "DeviceInstance\n" ++
             "        type = " ++ (deviceType d) ++ "\n" ++
             "        id = " ++ deviceInstanceID d ++ "\n"

getDeviceInstancesOfType :: Graph -> DeviceType -> [DeviceInstance]
getDeviceInstancesOfType g dt = filter (\i -> (deviceType i) == (deviceID dt)) dis
  where
    dis = getDeviceInstances g

data DeviceProperty = DeviceProperty
                    {
                        deviceProperty :: String,
                        value          :: String
                    } deriving Eq

instance Show DeviceProperty where
    show p = "DeviceProperty\n" ++
             "                " ++ show (deviceProperty p) ++
             "                value = " ++ value p

data EdgeInstance = EdgeInstance
                  {
                      inNode :: DeviceInstance,
                      outNode :: DeviceInstance
                  } deriving Eq

instance Show EdgeInstance where
    show e = "EdgeInstance\n" ++
             "        path = " ++ (deviceInstanceID $ inNode e) ++ ":in-"
                               ++ (deviceInstanceID $ outNode e) ++ ":out\n"

getEdgeInstances :: Graph -> [EdgeInstance]
getEdgeInstances g = edgeInstances $ getGraphInstance g
