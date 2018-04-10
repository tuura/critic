module Poets.Critic.Types (
  Graph (..), GraphType (..),
  MessageType (..), Message (..),
  DeviceType (..), State (..),
  InputPin (..), OutputPin (..),
  GraphInstance (..), DeviceInstance (..),
  EdgeInstance (..), getGraphType,
  getGraphInstance, getMessageTypes,
  getDeviceTypes, getDeviceInstances,
  getEdgeInstances
  ) where

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
                    states       :: [State],
                    inputPins    :: [InputPin],
                    outputPins   :: [OutputPin],
                    readyToSend  :: String
                } deriving Eq

instance Show DeviceType where
    show d = "DeviceType\n" ++
             "            " ++ deviceID d ++ "\n" ++
             "            " ++ (show $ states d) ++ "\n" ++
             "            " ++ (show $ inputPins d) ++ "\n" ++
             "            " ++ (show $ outputPins d) ++ "\n" ++
             "            ReadyToSend = " ++ readyToSend d ++ "\n"

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

data DeviceInstance = DeviceInstance
                    {
                        deviceType         :: String,
                        deviceInstanceID   :: String
                    } deriving Eq

instance Show DeviceInstance where
    show d = "DeviceInstance\n" ++
             "        type = " ++ deviceType d ++ "\n" ++
             "        id = " ++ deviceInstanceID d ++ "\n"

data EdgeInstance = EdgeInstance
                  {
                      inNode :: String,
                      outNode :: String
                  } deriving Eq

instance Show EdgeInstance where
    show e = "EdgeInstance\n" ++
             "        path = " ++ inNode e ++ ":in-" ++ outNode e ++ ":out\n"

getEdgeInstances :: Graph -> [EdgeInstance]
getEdgeInstances g = edgeInstances $ getGraphInstance g
