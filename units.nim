{.hint[XDeclaredButNotUsed]: off.}
import strutils, math, macros

template additive(quant: typedesc): typed =
  proc `+` *(x, y: quant): quant {.borrow.}
  proc `-` *(x, y: quant): quant {.borrow.}
  proc `+=` *(x: var quant, y: quant) {.borrow.}
  proc `-=` *(x: var quant, y: quant) {.borrow.}

template multiplicative(quant: typedesc): typed =
  proc `*` *(x: quant, y: float): quant {.borrow.}
  proc `*` *(x: float, y: quant): quant {.borrow.}
  proc `/` *(x: quant, y: float): quant {.borrow.}

  template `*=` *(x: var quant, y: float) = x = quant(float(x) * float(y))
  template `/=` *(x: var quant, y: float) = x = quant(float(x) / float(y))

template comparable(quant: typedesc): typed =
  proc `<`  *(x, y: quant): bool {.borrow.}
  proc `<=` *(x, y: quant): bool {.borrow.}
  proc `==` *(x, y: quant): bool {.borrow.}

template metricPrefix(unit: untyped, prefix: untyped, factor: untyped, addS = true): typed =
  template `prefix unit`*(x = 1.0): auto = (x * factor).unit
  when addS:
    template `prefix unit s`*(x = 1.0): auto = (x * factor).unit

template metricPrefixes(unit: untyped, addS = true): typed =
  metricPrefix(unit, yocto, 1e-24, addS)
  metricPrefix(unit, zepto, 1e-21, addS)
  metricPrefix(unit, atto,  1e-18, addS)
  metricPrefix(unit, femto, 1e-15, addS)
  metricPrefix(unit, pico,  1e-12, addS)
  metricPrefix(unit, nano,  1e-9, addS)
  metricPrefix(unit, micro, 1e-6, addS)
  metricPrefix(unit, milli, 1e-3, addS)
  metricPrefix(unit, centi, 1e-2, addS)
  metricPrefix(unit, deci,  1e-1, addS)
  metricPrefix(unit, deka,  1e1, addS)
  metricPrefix(unit, hecto, 1e2, addS)
  metricPrefix(unit, kilo,  1e3, addS)
  metricPrefix(unit, mega,  1e6, addS)
  metricPrefix(unit, giga,  1e9, addS)
  metricPrefix(unit, tera,  1e12, addS)
  metricPrefix(unit, peta,  1e15, addS)
  metricPrefix(unit, exa,   1e18, addS)
  metricPrefix(unit, zetta, 1e21, addS)
  metricPrefix(unit, yotta, 1e24, addS)

template quantity(quant: untyped, unit: untyped, output: untyped, addS = true): typed =
  type quant* = distinct float
  additive(quant)
  multiplicative(quant)
  comparable(quant)

  template unit*(x = 1.0): quant = quant(x)
  metricPrefixes(unit, addS)

  when addS:
    template `unit s`*(x = 1.0): quant = quant(x)

  when output == "kg":
    proc `$`*(x: quant): string = $ float(x / 1000).formatFloat(precision = 0) & " " & output
  else:
    proc `$`*(x: quant): string = $ float(x).formatFloat(precision = 0) & " " & output

template isMult(to: untyped, from1: untyped, from2: untyped): typed =
  proc `*` *(x: from1, y: from2): to {.borrow.}
  when not (from1 is from2):
    proc `*` *(x: from2, y: from1): to {.borrow.}
  proc `/` *(x: to, y: from1): from2 {.borrow.}
  when not (from1 is from2):
    proc `/` *(x: to, y: from2): from1 {.borrow.}

template isInverse(a: untyped, b: untyped): typed =
  proc `/` *(x: float, y: a): b {.borrow.}
  proc `/` *(x: float, y: b): a {.borrow.}

template isAlso(a: untyped, b: untyped): typed =
  template `as a` *(x: b): a = a(x)

macro `:=`(abc, data): typed =
  assert(abc.kind == nnkIdent)
  let to = $abc.ident

  case data.len
  of 0:
    let from1 = $data.ident
    result = parseStmt("isAlso(" & to & ", " & from1 & ")")
  of 3:
    assert(data.kind == nnkInfix)
    let m1 = data[0]
    assert(m1.kind == nnkIdent)

    let m2 = data[1]
    let from1 = case m2.kind
      of nnkIdent:
        $m2.ident
      else:
        $m2.intVal

    let m3 = data[2]
    assert(m3.kind == nnkIdent)
    let from2 = $m3.ident

    case $m1.ident
    of "*":
      result = parseStmt("isMult(" & to & ", " & from1 & ", " & from2 & ")")
    of "/":
      if from1 == "1":
        result = parseStmt("isInverse(" & to & ", " & from2 & ")")
      else:
        result = parseStmt("isMult(" & from1 & ", " & to & ", " & from2 & ")")
    else:
      assert false
  else:
    discard

template altName(x: untyped, y: untyped, addS = false): typed =
  const y* = x
  metricPrefixes(y, addS)

template inUnit(x: untyped, y: untyped): untyped =
  float(float(x).y)

# SI base units
quantity(Length, meter, "m")
meter.altName(metre, true)
quantity(Mass, gram, "kg")
quantity(Time, second, "s")
quantity(ElectricCurrent, ampere, "A")
quantity(Temperature, kelvin, "K")
quantity(AmountOfSubstance, mole, "mol")
quantity(LuminousIntensity, candela, "cd")

# Simple helpers
quantity(Area, squareMeter, "m²")
quantity(Volume, cubicMeter, "m³")

# SI named derived units
quantity(Angle, radian, "rad")
quantity(SolidAngle, steradian, "sr")
quantity(Frequency, hertz, "Hz")
quantity(Force, newton, "N")
quantity(Pressure, pascal, "Pa")
quantity(Energy, joule, "J")
quantity(Power, watt, "W")
quantity(ElectricCharge, coulomb, "C")
quantity(Potential, volt, "V")
quantity(Capacitance, farad, "F")
quantity(Resistance, ohm, "Ω")
quantity(Conductance, siemens, "S", false)
quantity(MagneticFlux, weber, "Wb")
quantity(MagneticFieldStrength, tesla, "T")
quantity(Inductance, henry, "H", false)
henry.altName(henries)
quantity(LuminousFlux, lumen, "lm")
quantity(Illuminance, lux, "lx", false)
quantity(Radioactivity, becquerel, "Bq")
quantity(AbsorbedDose, gray, "Gy")
quantity(EquivalentDose, sievert, "Sv")
quantity(CatalyticActivity, katal, "kat")

# Other derived units
quantity(Velocity, meterPerSecond, "m/s", false)
meterPerSecond.altName(metersPerSecond)
meterPerSecond.altName(metrePerSecond)
meterPerSecond.altName(metresPerSecond)

quantity(Acceleration, meterPerSecondSquared, "m/s²", false)
meterPerSecondSquared.altName(metersPerSecondSquared)
meterPerSecondSquared.altName(metrePerSecondSquared)
meterPerSecondSquared.altName(metresPerSecondSquared)

# Non-SI units
template degreeCelsius*(x: untyped): untyped = (x + 273.15).kelvin
template foot*(x: untyped): untyped = (x * 0.3048).meter
#template mile*(x: untyped): untyped = (x * 1_609.344).meter
#template liter*(x: untyped): untyped = (x / 1000).cubicmeter
#template gallon*(x: untyped): untyped = (x * 3.79).liter

# Relations between quantities
Area := Length * Length
Volume := Area * Length

Frequency := 1 / Time
Velocity := Length / Time
Acceleration := Velocity / Time

Angle := Length / Length
SolidAngle := Area / Area

Force := Mass * Acceleration
Pressure := Force / Area
Energy := Force * Length
Power := Energy / Time

ElectricCharge := Time * ElectricCurrent
Potential := Power / ElectricCurrent
Capacitance := ElectricCharge / Potential
Resistance := Potential / ElectricCurrent
Conductance := ElectricCurrent / Potential

MagneticFlux := Potential * Time
MagneticFieldStrength := MagneticFlux / Area
Inductance := MagneticFlux / ElectricCurrent

LuminousFlux := LuminousIntensity * SolidAngle
Illuminance := LuminousFlux / Area

Radioactivity := Frequency
EquivalentDose := Energy / Mass
AbsorbedDose := EquivalentDose
CatalyticActivity := Frequency * AmountOfSubstance

when isMainModule:
  # Tests
  var f = 2.newton
  var x = 2.meter
  var e = f * x
  echo e

  var v = 12.5.volt
  var i = 3.0.ampere
  var r = v / i
  var i2 = v / r
  echo r
  echo i2
  echo i2 == i

  var a = 2.meter * 5.meter
  echo a
  echo 2.meter * 5.meter * 3.meter

  echo 1.meter + 3.foot
  echo 3.yoctometer

  var s = 2.second
  echo s
  
  var vel = 2.meters / 5.second
  echo vel / millisecond

  var gew: Mass = 2.gram
  echo gew

  #var sum1 = 0.0
  #for i in 1..1000000000:
  #  sum1 += 1.0
  #echo sum1

  # Just as fast as simple floats
  #var sum2 = 0.0.kilogram
  #for i in 1..1000000000:
  #  sum2 += 1.0.kilogram
  #echo sum2

  var temp = 10.degreeCelsius
  echo temp + 10.kelvin

  echo x.inUnit(millimeters)
  echo x

  var ti: Time = 4.seconds
  echo ti
  var ve = 2.meters / ti
  echo ve
  var ac: Acceleration = ve / millisecond
  echo ac
  ac *= 3.0
  echo ac

  var eqv = 2.joule / 10.kilogram
  echo eqv
  echo asAbsorbedDose eqv

  echo 1 / 1.second
  echo asRadioactivity 1 / 1.second
  echo 20.kilogram
  echo 20.kilometer

  #echo 20.gallon

  #quantity(Efficiency, metersPe, "rad")
  #let fuelEfficiency = 40.mile / 1.gallon

  # TODO: Some more complicated examples for actual calculations
