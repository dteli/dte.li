
# MTA turnstile data

The first project at Metis is, as part of a team, to analyze turnstile data from New York City's MTA to figure out where to place people handing out tickets to a hypothetical gala supporting women in tech.

The MTA provides this data on a weekly basis, Saturday to Friday, so for a gala in the early summer we chose to look at the months of March, April, and May for the past two years, so, 2019 and 2018. So, let's actually look at the data...

Each turnstile is specified by a combination of control area ("C/A"), "UNIT", subunit channel position ("SCP"), and "STATION". Each also contains data on the lines that it serves, although we chose to not use this information. The turnstiles report usually in four-hour increments, with most of these lined up along the hour numbers divisible by 4. Let's look at some of the data for one of the turnstiles in the dataset.

```
           C/A  UNIT  ...  ENTRIES EXITS                                                               
0         A002  R051  ...  6527780                                            2210496                  
1         A002  R051  ...  6527794                                            2210507                  
2         A002  R051  ...  6527838                                            2210585                  
3         A002  R051  ...  6527994                                            2210644                  
4         A002  R051  ...  6528278                                            2210705                  
...        ...   ...  ...      ...                                                ...                  
5174109  TRAM2  R469  ...     5554                                                378                  
5174110  TRAM2  R469  ...     5554                                                378                  
5174111  TRAM2  R469  ...     5554                                                378                  
5174112  TRAM2  R469  ...     5554                                                378                  
5174113  TRAM2  R469  ...     5554                                                378
```

Well clearly this needs to be cleaned up. The "EXITS" header has a bunch of spaces after it, if nothing else. 

After we do some cleaning, we see that the turnstiles keep two counters that count up continuously until reset, one for entries and one for exits. The first thing to do with this data is get a diff of each cell with the previous. We can't just use a straight `.shift` because we don't want to compare across turnstiles, so as per a Stack Overflow answer, what we do is first mask all the cells that aren't duplicates of the above values in those columns that specify a turnstile, then use that mask to apply the `.where` operation.



```python
def add_ins_outs_to_df(df_in):
    df = df_in.copy()
    mask = df.duplicated(['CA', 'UNIT', 'SCP', 'STATION'])
    df['INS'] = np.where(mask, df['ENTRIES'] - df['ENTRIES'].shift(1), np.nan)
    df['OUTS'] = np.where(mask, df['EXITS'] - df['EXITS'].shift(1), np.nan)
    return df
```

I'm skipping some of the cleaning and processing steps here, but the short of it is that the turnstiles don't always behave like you'd expect them to. Some have jump of ridiculous amounts or even go backwards, as if they were reset or installed incorrectly. We had to account for that in our data processing, so we chose to drop all negative numbers and values that were too high to be reasonable.

After all of this cleaning and adding INS and OUTS, the first twenty rows of the data look like this.

```
      CA  UNIT       SCP STATION LINENAME DIVISION  ...     DESC  ENTRIES    EXITS            DATETIME    INS   OUTS
2   A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6527838  2210585 2018-02-24 11:00:00   44.0   78.0
3   A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6527994  2210644 2018-02-24 15:00:00  156.0   59.0
4   A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6528278  2210705 2018-02-24 19:00:00  284.0   61.0
5   A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6528389  2210728 2018-02-24 23:00:00  111.0   23.0
6   A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6528417  2210734 2018-02-25 03:00:00   28.0    6.0
7   A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6528423  2210741 2018-02-25 07:00:00    6.0    7.0
8   A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6528468  2210788 2018-02-25 11:00:00   45.0   47.0
9   A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6528591  2210838 2018-02-25 15:00:00  123.0   50.0
10  A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6528785  2210886 2018-02-25 19:00:00  194.0   48.0
11  A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6528890  2210905 2018-02-25 23:00:00  105.0   19.0
12  A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6528905  2210907 2018-02-26 03:00:00   15.0    2.0
13  A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6528918  2210938 2018-02-26 07:00:00   13.0   31.0
14  A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6529053  2211138 2018-02-26 11:00:00  135.0  200.0
15  A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6529268  2211202 2018-02-26 15:00:00  215.0   64.0
16  A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6530018  2211256 2018-02-26 19:00:00  750.0   54.0
17  A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6530289  2211291 2018-02-26 23:00:00  271.0   35.0
18  A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6530309  2211297 2018-02-27 03:00:00   20.0    6.0
19  A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6530322  2211331 2018-02-27 07:00:00   13.0   34.0
20  A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6530462  2211629 2018-02-27 11:00:00  140.0  298.0
21  A002  R051  02-00-00   59 ST  NQR456W      BMT  ...  REGULAR  6530676  2211684 2018-02-27 15:00:00  214.0   55.0
```

We added a DATETIME column that is of the datetime type used by pandas, since the DATE and TIME that came with the data were just strings. Notice also that the row that was labeled row 1 in the index is missing; this is because when we added INS and OUTS there was no previous row with which to diff, thus we end up with a `NaN` there and have to drop it.



We can group by dates to calculate the total INS for each turnstile over the course of every date, and then we can group to calculate totals for each station.

```python
def group_by_days(df):
    return df.groupby(['CA', 'UNIT', 'SCP', 'STATION', 'DATE']).agg({'INS': 'sum', 'OUTS': 'sum'})

def group_by_station(df):
    return df.groupby(['STATION','DATE']).agg({'INS':'sum', 'OUTS':'sum'})
```



Since the date is now in the index, we use `.get_level_values` to extract the array of dates (which we have processed from strings into datetimes) and list comprehension to produce a list of days of the week for each day, in the following function. Note we do this twice here, not that it's necessary: we started by using the `.dayofweek` property for each date, but then decided to go with the `.day_name` method. Anyway, now we group by the station and day of the week and aggregate using the median by default.

```python
def get_station_freqs(df, method='median'):
    df['DAY'] = [d.dayofweek for d in df.index.get_level_values('DATE')]
    df['DAYNAME'] = [d.day_name() for d in df.index.get_level_values('DATE')]
    return (df.groupby(['STATION', 'DAY','DAYNAME'])
              .agg({'INS':method, 'OUTS':method}))
```


To find out the mean weekday rankings, that is, the stations in order by their mean traffic across all weekdays, we first drop the weekends and then group by station, aggregating INS and OUTS by mean.

```python
def mean_weekday_rankings(df):
    df = df.drop(index=5, level='DAY')
    df = df.drop(index=6, level='DAY')
    return (df.groupby(['STATION'])
              .agg({'INS':'mean', 'OUTS':'mean'})
              .sort_values('INS', ascending=False))
```

Applying all of this, the top 15 highest-volume stations are as follows.

```
                      INS      OUTS
STATION                            
34 ST-PENN STA   171641.8  149048.0
GRD CNTRL-42 ST  156236.2  137458.3
23 ST            120611.1   86982.0
34 ST-HERALD SQ  116479.1  108184.5
14 ST-UNION SQ   105772.2   93012.0
FULTON ST        101452.4   85097.8
TIMES SQ-42 ST    99689.2   92261.2
42 ST-PORT AUTH   90768.4   73997.2
86 ST             84331.3   72739.9
125 ST            76559.1   60876.4
59 ST COLUMBUS    76224.3   61198.0
CANAL ST          71865.2   56979.1
96 ST             65934.5   49772.2
CHAMBERS ST       65757.5   47477.9
59 ST             65305.6   54559.8
47-50 STS ROCK    61115.4   64522.0
PATH NEW WTC      60075.4   54499.5
14 ST             59299.1   47778.1
FLUSHING-MAIN     58308.4   47063.0
50 ST             50921.6   40808.0
```




Alternatively, instead of grouping by station we can group by day to find out what weekdays have the most traffic across the whole network.

```python
def group_by_day_of_week(df, method=''):
    df = get_station_freqs(df)
    return df.groupby('DAY').agg({'INS':'sum','OUTS':'sum'})
```

This is comparable to an estimate I found that 4.something *unique* riders use the subway every day, if you assume that plenty of them are commuters. I actually would suspect more people to be commuters, using the subway twice daily, than is indicated by that estimate and our results.
The results being (with 0 as Monday):

```
           INS       OUTS
DAY                      
0    5609443.0  4256841.0
1    5968130.5  4564453.0
2    6016123.5  4598214.5
3    6048000.5  4625790.5
4    5857590.5  4505916.0
5    3404455.0  2777584.5
6    2595856.5  2139097.5
```

So we see Mondays a little less popular (though whether that correlates with people who might be interested in a tech gala is not certain) than the other weekdays, but not so much that it wouldn't be worth going out on Mondays necessarily. We also notice, as would be expected, that Saturdays and Sundays don't have as high of ridership, which also fits with the intuition that we wouldn't want to target tourists anyway.



Therefore, our group mainly focused on where to place the teams based on the top stations for weekdays. We did also consider weekends in the case that the teams aren't available during the week.




Lastly, we did a calculation asking which weeks in the year were most popular in 2018 and 2019. The range is about 12,000 between the lowest traffic and the highest (what we're looking at here is the average station volume summed across the week---yes, I need to get better at wording these things).

```
                INS           OUTS
WEEK
9     180986.489418  139653.531746
10    178258.592593  137729.566138
11    187829.637566  145348.558201
12    177098.703704  137532.917989
13    184201.888889  143306.923280
14    182078.243386  142110.436508
15    189127.687831  146886.415344
16    190055.804233  146508.526455
17    179572.455026  140133.825397
18    193744.005291  150376.240741
19    188750.693122  146003.682540
20    190974.588391  147935.189974
```

We didn't think this was statistically significant enough to include in our presentation, but here it is just in case you find it interesting.