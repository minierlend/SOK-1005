import numpy as np
import pandas as pd
import matplotlib.pyplot as plt



def annunitets():
    lanemengde = input('Hvor mye vil du låne? \n')
    lanemengde = float(lanemengde.replace(' ', ''))

    terminer = float(input('Hvor mange år vil du ha lånet? \n')) * 12

    rente = float(input('Hva er renten på lånet ditt? \n').replace(',', '.').replace('%', '')) / 100 / 12

    terminbelop = lanemengde * (rente * (1 + rente) ** terminer) / ((1 + rente) ** terminer - 1)


    tabell = []

    termin = 1


    while lanemengde > 0:
        m_rente = rente * lanemengde
        avdrag = terminbelop - m_rente
        lanemengde = lanemengde - avdrag
        tabell.append([termin, terminbelop, m_rente, avdrag, lanemengde])
        termin += 1

    inn = 1
 


    df = pd.DataFrame(tabell, columns=['Termin', 'Terminbeløp', 'Renter', 'Avdrag', 'Restgjeld']).astype(int)
    while (inn != 0 ):
        print(df.truncate(before=inn - 1, after=inn + 3).to_string(index=False))
        inn = int(input('\nHvilken termin vil du se? (0 for å avslutte)\n: '))

annunitets()





