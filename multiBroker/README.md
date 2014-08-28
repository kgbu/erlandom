# Multi Brokers network to implement neural system

see https://twitter.com/ocaokgbu/status/503730587140513792


Sensori-Motor system : Neurons are Brokers (meta-broker shall be required?)

## Sensor Broker(Nervs) Motors   --- Physical system

```
    S0       Br0       M0           Object0
    S1       Br1       M1           Object1
    S2       Br2       M2
    :        Brn       :
    :                  Mn
    :
    Sn     
```

```
  Sensor -> Broker (usually 1:N) tee-ing would be done in Broker network
  Broker -> Broker (N:M)
  Broker -> Motor  (N:1)
  Motor -> Object (usually N:1)
```

# Network is stable, but dynamic  
  
## Development of neural network

+ development phase (neural node is added continously)
+ En-weighting of neural network (used path is enhanced. nearby path may be suppressed)
+ least used neuron may be extinguished
+ connection is usually positive, but too near connection may cause inhibition gating (sharing signaling energy source would cause negative feedback for neghbours) <- energy/resouce sharing model may be applied 

## IO definition

```
S0 input (ADC) 16bit scalar, sampling rate 10msec 
   output : scalar
Broker (pshb) multiple input (some type of gate exists to cut off input from outer branch : scalar 
      output(message passing) value : scalar
Motor multiple input : scalar
      output DAC : sampling rate 1msec
```

## Physical model (recalc. by 1msec rate)



