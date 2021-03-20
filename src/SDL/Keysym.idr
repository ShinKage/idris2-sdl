module SDL.Keysym

import Data.Bits
import SDL.Elab

%default total
%language ElabReflection

public export
data Keycode
  = Unknown
  | Return
  | Escape
  | Backspace
  | Tab
  | Space
  | Exclaim
  | Quotedbl
  | Hash
  | Percent
  | Dollar
  | Ampersand
  | Quote
  | Leftparen
  | Rightparen
  | Asterisk
  | Plus
  | Comma
  | Minus
  | Period
  | Slash
  | N0
  | N1
  | N2
  | N3
  | N4
  | N5
  | N6
  | N7
  | N8
  | N9
  | Colon
  | Semicolon
  | Less
  | Equals
  | Greater
  | Question
  | At
  | Leftbracket
  | Backslash
  | Rightbracket
  | Caret
  | Underscore
  | Backquote
  | KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyE
  | KeyF
  | KeyG
  | KeyH
  | KeyI
  | KeyJ
  | KeyK
  | KeyL
  | KeyM
  | KeyN
  | KeyO
  | KeyP
  | KeyQ
  | KeyR
  | KeyS
  | KeyT
  | KeyU
  | KeyV
  | KeyW
  | KeyX
  | KeyY
  | KeyZ
  | Capslock
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | Printscreen
  | Scrolllock
  | Pause
  | Insert
  | Home
  | Pageup
  | Delete
  | End
  | Pagedown
  | Right
  | Left
  | Down
  | Up
  | NumlockClear
  | KpDivide
  | KpMultiply
  | KpMinus
  | KpPlus
  | KpEnter
  | Kp1
  | Kp2
  | Kp3
  | Kp4
  | Kp5
  | Kp6
  | Kp7
  | Kp8
  | Kp9
  | Kp0
  | KpPeriod
  | KpEquals
  | KpComma
  | Cancel
  | LCtrl
  | LShift
  | LAlt
  | LGui
  | RCtrl
  | RShift
  | RAlt
  | RGui

%runElab deriveUnitSumEq Export `{{Keycode}}
%runElab deriveUnitSumShow Export `{{Keycode}}

export
keycodeToInt : Keycode -> Int
keycodeToInt Unknown = 0x00000000
keycodeToInt Return = 0x0000000D
keycodeToInt Escape = 0x0000001B
keycodeToInt Backspace = 0x00000008
keycodeToInt Tab = 0x00000009
keycodeToInt Space = 0x00000020
keycodeToInt Exclaim = 0x00000021
keycodeToInt Quotedbl = 0x00000022
keycodeToInt Hash = 0x00000023
keycodeToInt Percent = 0x00000025
keycodeToInt Dollar = 0x00000024
keycodeToInt Ampersand = 0x00000026
keycodeToInt Quote = 0x00000027
keycodeToInt Leftparen = 0x00000028
keycodeToInt Rightparen = 0x00000029
keycodeToInt Asterisk = 0x0000002A
keycodeToInt Plus = 0x0000002B
keycodeToInt Comma = 0x0000002C
keycodeToInt Minus = 0x0000002D
keycodeToInt Period = 0x0000002E
keycodeToInt Slash = 0x0000002F
keycodeToInt N0 = 0x00000030
keycodeToInt N1 = 0x00000031
keycodeToInt N2 = 0x00000032
keycodeToInt N3 = 0x00000033
keycodeToInt N4 = 0x00000034
keycodeToInt N5 = 0x00000035
keycodeToInt N6 = 0x00000036
keycodeToInt N7 = 0x00000037
keycodeToInt N8 = 0x00000038
keycodeToInt N9 = 0x00000039
keycodeToInt Colon = 0x0000003A
keycodeToInt Semicolon = 0x0000003B
keycodeToInt Less = 0x0000003C
keycodeToInt Equals = 0x0000003D
keycodeToInt Greater = 0x0000003E
keycodeToInt Question = 0x0000003F
keycodeToInt At = 0x00000040
keycodeToInt Leftbracket = 0x0000005B
keycodeToInt Backslash = 0x0000005C
keycodeToInt Rightbracket = 0x0000005D
keycodeToInt Caret = 0x0000005E
keycodeToInt Underscore = 0x0000005F
keycodeToInt Backquote = 0x00000060
keycodeToInt KeyA = 0x00000061
keycodeToInt KeyB = 0x00000062
keycodeToInt KeyC = 0x00000063
keycodeToInt KeyD = 0x00000064
keycodeToInt KeyE = 0x00000065
keycodeToInt KeyF = 0x00000066
keycodeToInt KeyG = 0x00000067
keycodeToInt KeyH = 0x00000068
keycodeToInt KeyI = 0x00000069
keycodeToInt KeyJ = 0x0000006A
keycodeToInt KeyK = 0x0000006B
keycodeToInt KeyL = 0x0000006C
keycodeToInt KeyM = 0x0000006D
keycodeToInt KeyN = 0x0000006E
keycodeToInt KeyO = 0x0000006F
keycodeToInt KeyP = 0x00000070
keycodeToInt KeyQ = 0x00000071
keycodeToInt KeyR = 0x00000072
keycodeToInt KeyS = 0x00000073
keycodeToInt KeyT = 0x00000074
keycodeToInt KeyU = 0x00000075
keycodeToInt KeyV = 0x00000076
keycodeToInt KeyW = 0x00000077
keycodeToInt KeyX = 0x00000078
keycodeToInt KeyY = 0x00000079
keycodeToInt KeyZ = 0x0000007A
keycodeToInt Capslock = 0x40000039
keycodeToInt F1 = 0x4000003A
keycodeToInt F2 = 0x4000003B
keycodeToInt F3 = 0x4000003C
keycodeToInt F4 = 0x4000003D
keycodeToInt F5 = 0x4000003E
keycodeToInt F6 = 0x4000003F
keycodeToInt F7 = 0x40000040
keycodeToInt F8 = 0x40000041
keycodeToInt F9 = 0x40000042
keycodeToInt F10 = 0x40000043
keycodeToInt F11 = 0x40000044
keycodeToInt F12 = 0x40000045
keycodeToInt Printscreen = 0x40000046
keycodeToInt Scrolllock = 0x40000047
keycodeToInt Pause = 0x40000048
keycodeToInt Insert = 0x40000049
keycodeToInt Home = 0x4000004A
keycodeToInt Pageup = 0x4000004B
keycodeToInt Delete = 0x4000004C
keycodeToInt End = 0x4000004D
keycodeToInt Pagedown = 0x4000004E
keycodeToInt Right = 0x4000004F
keycodeToInt Left = 0x40000050
keycodeToInt Down = 0x40000051
keycodeToInt Up = 0x40000052
keycodeToInt NumlockClear = 0x40000053
keycodeToInt KpDivide = 0x40000054
keycodeToInt KpMultiply = 0x40000055
keycodeToInt KpMinus = 0x40000056
keycodeToInt KpPlus = 0x40000057
keycodeToInt KpEnter = 0x40000058
keycodeToInt Kp1 = 0x40000059
keycodeToInt Kp2 = 0x4000005A
keycodeToInt Kp3 = 0x4000005B
keycodeToInt Kp4 = 0x4000005C
keycodeToInt Kp5 = 0x4000005D
keycodeToInt Kp6 = 0x4000005E
keycodeToInt Kp7 = 0x4000005F
keycodeToInt Kp8 = 0x40000060
keycodeToInt Kp9 = 0x40000061
keycodeToInt Kp0 = 0x40000062
keycodeToInt KpPeriod = 0x40000063
keycodeToInt KpEquals = 0x40000067
keycodeToInt KpComma = 0x40000085
keycodeToInt Cancel = 0x4000009B
keycodeToInt LCtrl = 0x400000E0
keycodeToInt LShift = 0x400000E1
keycodeToInt LAlt = 0x400000E2
keycodeToInt LGui = 0x400000E3
keycodeToInt RCtrl = 0x400000E4
keycodeToInt RShift = 0x400000E5
keycodeToInt RAlt = 0x400000E6
keycodeToInt RGui = 0x400000E7

export
keycodeFromInt : Int -> Keycode
keycodeFromInt 0x0000000D = Return
keycodeFromInt 0x0000001B = Escape
keycodeFromInt 0x00000008 = Backspace
keycodeFromInt 0x00000009 = Tab
keycodeFromInt 0x00000020 = Space
keycodeFromInt 0x00000021 = Exclaim
keycodeFromInt 0x00000022 = Quotedbl
keycodeFromInt 0x00000023 = Hash
keycodeFromInt 0x00000025 = Percent
keycodeFromInt 0x00000024 = Dollar
keycodeFromInt 0x00000026 = Ampersand
keycodeFromInt 0x00000027 = Quote
keycodeFromInt 0x00000028 = Leftparen
keycodeFromInt 0x00000029 = Rightparen
keycodeFromInt 0x0000002A = Asterisk
keycodeFromInt 0x0000002B = Plus
keycodeFromInt 0x0000002C = Comma
keycodeFromInt 0x0000002D = Minus
keycodeFromInt 0x0000002E = Period
keycodeFromInt 0x0000002F = Slash
keycodeFromInt 0x00000030 = N0
keycodeFromInt 0x00000031 = N1
keycodeFromInt 0x00000032 = N2
keycodeFromInt 0x00000033 = N3
keycodeFromInt 0x00000034 = N4
keycodeFromInt 0x00000035 = N5
keycodeFromInt 0x00000036 = N6
keycodeFromInt 0x00000037 = N7
keycodeFromInt 0x00000038 = N8
keycodeFromInt 0x00000039 = N9
keycodeFromInt 0x0000003A = Colon
keycodeFromInt 0x0000003B = Semicolon
keycodeFromInt 0x0000003C = Less
keycodeFromInt 0x0000003D = Equals
keycodeFromInt 0x0000003E = Greater
keycodeFromInt 0x0000003F = Question
keycodeFromInt 0x00000040 = At
keycodeFromInt 0x0000005B = Leftbracket
keycodeFromInt 0x0000005C = Backslash
keycodeFromInt 0x0000005D = Rightbracket
keycodeFromInt 0x0000005E = Caret
keycodeFromInt 0x0000005F = Underscore
keycodeFromInt 0x00000060 = Backquote
keycodeFromInt 0x00000061 = KeyA
keycodeFromInt 0x00000062 = KeyB
keycodeFromInt 0x00000063 = KeyC
keycodeFromInt 0x00000064 = KeyD
keycodeFromInt 0x00000065 = KeyE
keycodeFromInt 0x00000066 = KeyF
keycodeFromInt 0x00000067 = KeyG
keycodeFromInt 0x00000068 = KeyH
keycodeFromInt 0x00000069 = KeyI
keycodeFromInt 0x0000006A = KeyJ
keycodeFromInt 0x0000006B = KeyK
keycodeFromInt 0x0000006C = KeyL
keycodeFromInt 0x0000006D = KeyM
keycodeFromInt 0x0000006E = KeyN
keycodeFromInt 0x0000006F = KeyO
keycodeFromInt 0x00000070 = KeyP
keycodeFromInt 0x00000071 = KeyQ
keycodeFromInt 0x00000072 = KeyR
keycodeFromInt 0x00000073 = KeyS
keycodeFromInt 0x00000074 = KeyT
keycodeFromInt 0x00000075 = KeyU
keycodeFromInt 0x00000076 = KeyV
keycodeFromInt 0x00000077 = KeyW
keycodeFromInt 0x00000078 = KeyX
keycodeFromInt 0x00000079 = KeyY
keycodeFromInt 0x0000007A = KeyZ
keycodeFromInt 0x40000039 = Capslock
keycodeFromInt 0x4000003A = F1
keycodeFromInt 0x4000003B = F2
keycodeFromInt 0x4000003C = F3
keycodeFromInt 0x4000003D = F4
keycodeFromInt 0x4000003E = F5
keycodeFromInt 0x4000003F = F6
keycodeFromInt 0x40000040 = F7
keycodeFromInt 0x40000041 = F8
keycodeFromInt 0x40000042 = F9
keycodeFromInt 0x40000043 = F10
keycodeFromInt 0x40000044 = F11
keycodeFromInt 0x40000045 = F12
keycodeFromInt 0x40000046 = Printscreen
keycodeFromInt 0x40000047 = Scrolllock
keycodeFromInt 0x40000048 = Pause
keycodeFromInt 0x40000049 = Insert
keycodeFromInt 0x4000004A = Home
keycodeFromInt 0x4000004B = Pageup
keycodeFromInt 0x4000004C = Delete
keycodeFromInt 0x4000004D = End
keycodeFromInt 0x4000004E = Pagedown
keycodeFromInt 0x4000004F = Right
keycodeFromInt 0x40000050 = Left
keycodeFromInt 0x40000051 = Down
keycodeFromInt 0x40000052 = Up
keycodeFromInt 0x40000053 = NumlockClear
keycodeFromInt 0x40000054 = KpDivide
keycodeFromInt 0x40000055 = KpMultiply
keycodeFromInt 0x40000056 = KpMinus
keycodeFromInt 0x40000057 = KpPlus
keycodeFromInt 0x40000058 = KpEnter
keycodeFromInt 0x40000059 = Kp1
keycodeFromInt 0x4000005A = Kp2
keycodeFromInt 0x4000005B = Kp3
keycodeFromInt 0x4000005C = Kp4
keycodeFromInt 0x4000005D = Kp5
keycodeFromInt 0x4000005E = Kp6
keycodeFromInt 0x4000005F = Kp7
keycodeFromInt 0x40000060 = Kp8
keycodeFromInt 0x40000061 = Kp9
keycodeFromInt 0x40000062 = Kp0
keycodeFromInt 0x40000063 = KpPeriod
keycodeFromInt 0x40000067 = KpEquals
keycodeFromInt 0x40000085 = KpComma
keycodeFromInt 0x4000009B = Cancel
keycodeFromInt 0x400000E0 = LCtrl
keycodeFromInt 0x400000E1 = LShift
keycodeFromInt 0x400000E2 = LAlt
keycodeFromInt 0x400000E3 = LGui
keycodeFromInt 0x400000E4 = RCtrl
keycodeFromInt 0x400000E5 = RShift
keycodeFromInt 0x400000E6 = RAlt
keycodeFromInt 0x400000E7 = RGui
keycodeFromInt x = Unknown

namespace Keymod

  public export
  data Keymod
    = LShift
    | RShift
    | LCtrl
    | RCtrl
    | LAlt
    | RAlt
    | LGui
    | RGui
    | Num
    | Caps
    | Mode

  %runElab deriveUnitSumEq Export `{{Keymod}}
  %runElab deriveUnitSumShow Export `{{Keymod}}

  keymodMask : Keymod -> Bits16
  keymodMask LShift = 0x0001
  keymodMask RShift = 0x0002
  keymodMask LCtrl = 0x0040
  keymodMask RCtrl = 0x0080
  keymodMask LAlt = 0x0100
  keymodMask RAlt = 0x0200
  keymodMask LGui = 0x0400
  keymodMask RGui = 0x0800
  keymodMask Num = 0x1000
  keymodMask Caps = 0x2000
  keymodMask Mode = 0x4000

  export
  keymodsToBits : List Keymod -> Bits16
  keymodsToBits mods = foldl (.|.) 0 (keymodMask <$> mods)

  export
  keymodsFromBits : Bits16 -> List Keymod
  keymodsFromBits mods = foldl convert [] masks
    where
      convert : List Keymod -> (Keymod, Bits16) -> List Keymod
      convert acc (mod, mask) = if mask .&. mods /= 0 then mod :: acc else acc

      masks : List (Keymod, Bits16)
      masks = map (\m => (m, keymodMask m)) [LShift, RShift, LCtrl, RCtrl, LAlt, RAlt, LGui, RGui, Num, Caps, Mode]
