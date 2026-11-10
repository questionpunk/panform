---
title: Customer feedback
title@fr: Avis client
languages: en, fr
---

# {#p1}

1. Have you used our product? {#used *}
   @fr Avez-vous utilisé notre produit ?
   ( ) Yes {#yes}
   @fr Oui
   ( ) No {#no}
   @fr Non

2. Which features do you use? {#features}
   if used = yes
   [ ] reports
   [ ] exports
   [ ] api
   [ ] Other {#other other}
   [ ] None {#none only}

3. Which language do you call the API from? {#api_lang}
   if features has api

# Satisfaction {#p2}
   if used = yes

4. How satisfied are you with... {#sat}
   - Speed {#speed}
   - Price {#price}
   ( ) Unhappy {#1}
   ( ) Neutral {#2}
   ( ) Happy {#3}

5. How likely are you to recommend us? {#nps}
   [0] Not likely .. [10] Very likely

6. Age {#age number}
   min 18
   max 120
   ! Must be 18-120

7. Why? {#why long}
   if nps <= 6 and nps is answered
