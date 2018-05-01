module EVM.Insn
    ( Insn(..)
    , encodeInsn
    , encodeProgram
    ) where

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder (Builder, toLazyByteString, word8, lazyByteString)
import Data.Monoid ((<>))
import Data.Word (Word8)

data Insn
    -- Stop and Arithmetic ops
    = IStop
    | IAdd
    | IMul
    | ISub
    | IDiv
    | ISDiv
    | IMod
    | ISMod
    | IAddMod
    | IMulMod
    | IExp
    | ISignExtend
    -- Comparison & Bitwise Logic Ops
    | ILT
    | IGT
    | ISLT
    | ISGT
    | IEQ
    | IIsZero
    | IAnd
    | IOr
    | IXor
    | INot
    | IByte
    -- SHA3
    | ISHA3
    -- Environment Information Ops
    | IAddress
    | IBalance
    | IOrigin
    | ICaller
    | ICallValue
    | ICallDataLoad
    | ICallDataSize
    | ICallDataCopy
    | ICodeSize
    | ICodeCopy
    | IGasPrice
    | IExtCodeSize
    | IExtCodeCopy
    | IReturnDataSize
    | IReturnDataCopy
    -- Block Information Ops
    | IBlockHash
    | ICoinbase
    | ITimestamp
    | INumber
    | IDifficulty
    | IGasLimit
    -- Stack, Memory, Storage & Flow Ops
    | IPop
    | IMLoad
    | IMStore
    | IMStore8
    | ISLoad
    | ISStore
    | IJump
    | IJumpI
    | IPC
    | IMSize
    | IGas
    | IJumpDest
    -- Push Ops
    | IPush Word8 Builder
    -- Duplication Ops
    | IDup Word8
    -- Exchange Ops
    | ISwap Word8
    -- Logging Ops
    | ILog Word8
    -- System Ops
    | ICreate
    | ICall
    | ICallCode
    | IReturn
    | IDelegateCall
    | IStaticCall
    | IRevert
    | IInvalid
    -- Halt & mark for delete
    | ISelfDestruct

encodeInsn :: Insn -> Builder
encodeInsn (IPush n bytes) = word8 (0x60 + n - 1) <> bytes
encodeInsn insn = word8 $ case insn of
    IStop           -> 0x00
    IAdd            -> 0x01
    IMul            -> 0x02
    ISub            -> 0x03
    IDiv            -> 0x04
    ISDiv           -> 0x05
    IMod            -> 0x06
    ISMod           -> 0x07
    IAddMod         -> 0x08
    IMulMod         -> 0x09
    IExp            -> 0x0a
    ISignExtend     -> 0x0b
    ILT             -> 0x10
    IGT             -> 0x11
    ISLT            -> 0x12
    ISGT            -> 0x13
    IEQ             -> 0x14
    IIsZero         -> 0x15
    IAnd            -> 0x16
    IOr             -> 0x17
    IXor            -> 0x18
    INot            -> 0x19
    IByte           -> 0x1a
    ISHA3           -> 0x20
    IAddress        -> 0x30
    IBalance        -> 0x31
    IOrigin         -> 0x32
    ICaller         -> 0x33
    ICallValue      -> 0x34
    ICallDataLoad   -> 0x35
    ICallDataSize   -> 0x36
    ICallDataCopy   -> 0x37
    ICodeSize       -> 0x38
    ICodeCopy       -> 0x39
    IGasPrice       -> 0x3a
    IExtCodeSize    -> 0x3b
    IExtCodeCopy    -> 0x3c
    IReturnDataSize -> 0x3d
    IReturnDataCopy -> 0x3e
    IBlockHash      -> 0x40
    ICoinbase       -> 0x41
    ITimestamp      -> 0x42
    INumber         -> 0x43
    IDifficulty     -> 0x44
    IGasLimit       -> 0x45
    IPop            -> 0x50
    IMLoad          -> 0x51
    IMStore         -> 0x52
    IMStore8        -> 0x53
    ISLoad          -> 0x54
    ISStore         -> 0x55
    IJump           -> 0x56
    IJumpI          -> 0x57
    IPC             -> 0x58
    IMSize          -> 0x59
    IGas            -> 0x5a
    IJumpDest       -> 0x5b
    IDup n          -> 0x80 + n - 1
    ISwap n         -> 0x90 + n - 1
    ICreate         -> 0xf0
    ICall           -> 0xf1
    ICallCode       -> 0xf2
    IReturn         -> 0xf3
    IDelegateCall   -> 0xf4
    IStaticCall     -> 0xfa
    IRevert         -> 0xfd
    IInvalid        -> 0xfe
    ISelfDestruct   -> 0xff

encodeProgram :: Foldable t => t Insn -> B.ByteString
encodeProgram = toLazyByteString . foldMap encodeInsn
