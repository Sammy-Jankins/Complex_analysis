# Complex-analysis

Dieses Projekt gibt eine kurze Einführung in das Thema "künstliche neuronale Netze" (Vorkenntnisse: Schulwissen).
Ein Kurzskript zum Thema befindet sich in "KNN.pdf". Im Ordner "Notebooks" befinden sich die zugehörigen Python-Notebooks, die man z.B. in google Drive mit 'Colaboratory' öffnen kann.

Version:
```
Python (3.6.8)
```


Notebooks: 

- KNN1.ipynb: siehe Abschnitt 3.2 im Skript.
- KNN2.ipynb: siehe Abschnitt 4.3 im Skript.
- KNN3.ipynb: Klassifikation, siehe Abschnitt 4 im Skript. Alternativ: https://playground.tensorflow.org.
- KNN4.ipynb: Bilderkennung: Es werden KNN trainiert, die handgeschriebene Ziffern (28 x 28-Pixel) erkennen können (MNIST-Datensatz).
- KNN5.ipynb: GAN zum MNIST-Datensatz.
- KNN6.ipynb: Autoencoder
- KNN7.ipynb: Reinforcement learning: Ein KNN lernt, Pong zu spielen. (Low-Level-API-Implementierung: KNN7_low_level_API.ipynb)

-----

<b>Bilder:</b>

KNN4: Auf diesem Bild erkennt das KNN die Ziffer 3 (mit Wahrscheinlichkeit 57%).

<img src = "./Bilder/loewe.png" width=800>

Neu generierte Bilder von Ziffern (KNN5):

<img src = "./Bilder/fake_digits.png" width=200>

Ein KNN spielt Pong (KNN7), links vor dem Training, rechts nach ca. 12 Stunden Training:


<img src = "./Bilder/pong_nn_small-1.gif" width=200>  &nbsp;   &nbsp;   &nbsp;   &nbsp;   &nbsp;      <img src = "./Bilder/pong_nn_small-4.gif" width=200>

