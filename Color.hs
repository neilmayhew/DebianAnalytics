module Color
        (hsl2rgb, hsl, hsv2rgb, hsv)
where

import Text.Printf
import Test.HUnit

-- h: [0..360) s: [0..1] l: [0..1]
-- r,g,b: [0..1]
hsl2rgb :: (Double, Double, Double) -> (Double, Double, Double)
hsl2rgb (h, s, l) =
    let q = if l <= 0.5 then l*(1+s) else l*(1-s) + s
        p = 2*l - q
    in (rgbQuant (p, q, h+120),
        rgbQuant (p, q, h    ),
        rgbQuant (p, q, h-120))
  where
    rgbQuant (p, q, h') =
        let h = if      h' <    0 then h' + 360
                else if h' >= 360 then h' - 360
                else                   h'
        in      if      h  <   60 then p + (q-p)*h/60
                else if h  <  180 then q
                else if h  <  240 then p + (q-p)*(240-h)/60
                else                   p

-- h: [0..360) s: [0..1] v: [0..1]
-- r,g,b: [0..1]
hsv2rgb :: (Double, Double, Double) -> (Double, Double, Double)
hsv2rgb (h, s, v) =
    let (i, f) = properFraction (h / 60)
        p = v * (1 - s);
        q = v * (1 - s * f);
        t = v * (1 - s * (1 - f));
        u = v
    in case i of
        0 -> (u, t, p)
        1 -> (q, u, p)
        2 -> (p, u, t)
        3 -> (p, q, u)
        4 -> (t, p, u)
        _ -> (u, p, q)

-- h: [0..360) s: [0..100] l: [0..100]
hsl :: (Double, Double, Double) -> String
hsl (h, s, l) = css $ hsl2rgb (h, s/100, l/100)

-- h: [0..360) s: [0..100] v: [0..100]
hsv :: (Double, Double, Double) -> String
hsv (h, s, v) = css $ hsv2rgb (h, s/100, v/100)

css :: (Double, Double, Double) -> String
css (r, g, b) = printf "#%02X%02X%02X" (r'::Int) (g'::Int) (b'::Int)
  where (r', g', b') = (round $ 255 * r, round $ 255 * g, round $ 255 * b)

tests = TestList
    [ "#566973" ~=? hsv(200,0.25,0.45)
    , "#FFFF80" ~=? hsv( 60,0.50,1.00)
    , "#EBF8FF" ~=? hsv(200,0.08,1.00)
    , "#175473" ~=? hsv(200,0.80,0.45)
    ]
