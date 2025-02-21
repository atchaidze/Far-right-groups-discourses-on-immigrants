setwd("C:/Users/crrc_/OneDrive/Documents/PHD thesis/PHD/PHD TSU/Thesis/data/Final data collection")
library(tidyverse)
library(haven)

library(lubridate)
library(openxlsx)
library(ggplot2)
library(ggpubr)
library(questionr)
library(pollster)
library(gmodels)
library(pivottabler)
library(viridis)
library(hrbrthemes)
library(gridExtra)
library(extrafont)
library(readr)

data<-read_csv("200k_posts_raw.csv")
data_small<-read_csv("481_post_from 42 pages.csv")

matching_posts <- data_small$URL %in% data$URL
table(matching_posts)


## remove unnecessary pages from data ____________ Phase 1
drop_pages<-as.data.frame(sort(table(data$`Page Name`), decreasing = TRUE) [1:20])
#write.xlsx(drop_pages,"C:/Users/crrc_/OneDrive/Documents/PHD thesis/PHD/PHD TSU/Thesis/data/Final data collection/drop unnecessary pages/drop_unnececarypages.xlsx")
specified_pages <- c("Home shop N•28",
                     "www.itv.ge",
                     'ბიძინა ივანიშვილის მეგობრები "ფეისბუქზე"',
                     "7ბ ბუტიკი - 7B Boutique",
                     "პროტესტი",
                     "Newposts.ge",
                     "ambebi.ge • ამბები.ge",
                     "intermedia.ge",
                     "ჩვენ ამერიკაში",
                     "ყველაზე მთავარია - თავისუფლება",
                     "რუსთავი 21-👁️-RUSTAVI 21",
                     "ონლაინ შოპინგი დაბალ ფასად • Online shoppingi dabal fasad",
                     "რეპორტიორი",
                     "კახა კალაძე თბილისის მერი",
                     "ყველა სიახლე",
                     "ნიჭიერთა სამყარო",
                     "კვირის პალიტრა • kvirispalitra.ge",
                     "TV Imedi",
                     "ემიგრანტები იტალიაში / LA MIA GEORGIA",
                     "რადიო თავისუფლება")

cleaned_data <- data[!(data$`Page Name` %in% specified_pages), ]



## remove unnecessary pages from data ____________ Phase 2
drop_pages<-as.data.frame(sort(table(cleaned_data$`Page Name`), decreasing = TRUE) [1:20])
drop_pages
#write.xlsx(drop_pages,"C:/Users/crrc_/OneDrive/Documents/PHD thesis/PHD/PHD TSU/Thesis/data/Final data collection/drop unnecessary pages/drop_unnececarypages2.xlsx")
specified_pages <- c("მილე-New-მი",
"Tabula - ტაბულა",
"Prime Time • პრაიმ ტაიმი",
"ჩემი გორი",
"მოგზაურთა კლუბი - Cheap Travel Georgia",
"პირველი არხი",
"Myauto | მანქანების ყიდვა - გაყიდვა - გაქირავება",
"BMG • ბიემჯი",
"Georgischer Verein in Deutschland e. V. / გერმანიის ქართული სათვისტომო",
"Mediamall.ge",
"აქ ყველაფერია/Mall",
"აქ ქირავდება/გირავდება/იყიდება ბინები - Georgian.ge",
"Newshub.ge - ნიუსჰაბი",
"ქართველი ემიგრანტები",
"palitravideo.ge",
"ქართველები ამერიკაში",
"ექსკლუზივი - Exclusive",
"InterPressNews.ge",
"TV პალიტრანიუსი - TV PalitraNews",
"Radio Fortuna FM 106,9")

cleaned_data <- cleaned_data[!(cleaned_data$`Page Name` %in% specified_pages), ]




## remove unnecessary pages from data ____________ Phase 3
drop_pages<-as.data.frame(sort(table(cleaned_data$`Page Name`), decreasing = TRUE) [1:20])
drop_pages
#write.xlsx(drop_pages,"C:/Users/crrc_/OneDrive/Documents/PHD thesis/PHD/PHD TSU/Thesis/data/Final data collection/drop unnecessary pages/drop_unnececarypages3.xlsx")
specified_pages <- c("ბინების ყიდვა / გაყიდვა / გაქირავება / დაგირავება",
                     'მედიაჰოლდინგი "კვირა"',
                     "გავერთიანდეთ მტრის წინააღმდეგ და მიშასეული წინსვლის დასაბრუნებლად!",
                     "Rustavi 2 • რუსთავი 2",
                     "ყიდვა გაყიდვა გაქირავება",
                     "All Market ყიდვა გაყიდვა გაცვლა გაქირავება",
                     "ალია",
                     "მკითხველთა ლიგა 🕊",
                     "მიუსაფარ ცხოველთა კლუბი●Homeless Pets Club",
                     "ამერიკის ხმა",
                     "მილიონი ქართველი",
                     "bpn.ge • ბიზნესპრესნიუსი",
                     "Ipress.ge",
                     "Sputnik საქართველო: ახალი ამბები",
                     "ბინების ყიდვა გაყიდვა გაქირავება დაგირავება.",
                     "ყიდვა/გაყიდვა/გაქირავება საქართველოში",
                     "Georgian.ge | უძრავი ქონების განცხადებები - Apartments in Tbilisi/Batumi",
                     "გაყიდვები თბილისში  [ Tbilisi Deals ]",
                     "შენი სახლი • Sheni Sakhli",
                     "და ქალი")

cleaned_data <- cleaned_data[!(cleaned_data$`Page Name` %in% specified_pages), ]


## remove unnecessary pages from data ____________ Phase 4
drop_pages<-as.data.frame(sort(table(cleaned_data$`Page Name`), decreasing = TRUE) [1:40])
drop_pages
#write.xlsx(drop_pages,"C:/Users/crrc_/OneDrive/Documents/PHD thesis/PHD/PHD TSU/Thesis/data/Final data collection/drop unnecessary pages/drop_unnececarypages4.xlsx")
specified_pages <- c("ემიგრანტის პირადი სივრცე",
                     "Cyber Marketing/კიბერ მარკეტინგი",
                     'პოლიტიკური კოალიცია "ქართული ოცნება"-ს მხარდამჭერთა ჯგუფი',
                     "Netgazeti",
                     "News.Ge",
                     "კომერსანტი • Commersant",
                     "Metronome.Ge",
                     "Allnews.ge • ოლნიუსი",
                     "საინფორმაციო სააგენტო INFO9",
                     "NEWS.On.ge",
                     "girls' best choice",
                     "ალფა სახლი",
                     "ჩვენი ხმა ბიძინა ივანიშვილს",
                     "IN KuTaiSi",
                     "Edemi ედემი",
                     "მანქანების ავტომობილების ყიდვა გაყიდვა და გაცვლა",
                     "ადამიანები მსოფლიოს ქალაქებიდან",
                     "RUSTAVI რუსთავი",
                     "ქუჩის ძაღლები❤️🐾",
                     "ბინები ქირით • BiNeBi QIRIT",
                     "YIDVA GAYIDVA ..GACVLA.GANCXADEBA",
                     "History of Georgia",
                     "MBG Group / ემბიჯი ჯგუფი - უძრავი ქონების სააგენტო",
                     "რუსთავი და რუსთაველები",
                     "ყიდვა გაყიდვა საქართველოში 🇬🇪",
                     "წიგნის ჭიები 📚",
                     "საქართველო და მსოფლიო",
                     "დემოკრატიული ქოლგა",
                     "iMedia Group",
                     "მობილურების და პლანშეტების ყიდვა გაყიდვა",
                     "ცხოველთა უფლებების კომიტეტი Animal Rights Commettee of Georgia",
                     "დამზადებულია საქართველოში",
                     "დაარეკლამე რაც გინდა",
                     "ჩვენი სამყარო ツ",
                     "1001 ფილმი რომელიც უნდა ნახო სანამ ცოცხალი ხარ",
                     "გვიხარია ცხოვრება",
                     "ქართული დიასპორა იტალიაში/Diaspora georgiana in Italia/",
                     "მთავარი არხი • Mtavari Arkhi",
                     "ყიდვა-გაყიდვა “ქუთაისი” yidva gayidva",
                     "აჭარის ტელევიზია • Ajara TV  - საზოგადოებრივი მაუწყებელი")

cleaned_data <- cleaned_data[!(cleaned_data$`Page Name` %in% specified_pages), ]



## remove unnecessary pages from data ____________ Phase 5
drop_pages<-as.data.frame(sort(table(cleaned_data$`Page Name`), decreasing = TRUE) [1:60])
drop_pages
#write.xlsx(drop_pages,"C:/Users/crrc_/OneDrive/Documents/PHD thesis/PHD/PHD TSU/Thesis/data/Final data collection/drop unnecessary pages/drop_unnececarypages5.xlsx")
specified_pages <- c("ბათუმი - ყიდვა, გაყიდვა, გაქირავება / ყიდვა გაყიდვა გაქირავება ბათუმში",
                     "ყიდვა-გაყიდვა ბათუმში",
                     "Барахолка для Тбилисцев",
                     "Business Cafe - ბიზნეს კაფე",
                     "აკაკი წერეთლის სახელმწიფო უნივერსიტეტი /   Akaki Tsereteli State University",
                     "ავტომობილების ყიდვა - გაყიდვა საქართველოში",
                     "ედემი შოპი • Edemi Shop",
                     "Gogaggg",
                     "On.ge",
                     "სპორტი ყველასია",
                     "ბათუმელები • Batumelebi.ge",
                     "FashionHolics Online",
                     "ქართული გვერდი",
                     "IMEDINEWS",
                     "Недвижимость Грузии💸Real estate in Georgia💕უძრავი ქონება საქართველოში",
                     "POSTV - ახალი ამბები",
                     "იტალიის ცის ქვეშ – L'ANIMA GEORGIANA",
                     "ისტორია",
                     "უძრავი ქონების ყიდვა გაყიდვა, გაქირავება ,დაგირავება. udzravi qoneba",
                     "კვირის სიახლეები",
                     "დაიჯესტი • Digest",
                     "დევნილები დევნილებისთვის",
                     "უძრავი ქონების ყიდვა-გაყიდვა საქართველოში",
                     "პოზიტივი",
                     "ბარახოლკა-ბათუმი � � �BARAXOLKA---BATUMI",
                     "მალალუ - Malalu - ონლაინ შოპინგი",
                     "ემიგრანტის დღიურები ამერიკიდან(Immigrant's diaries from America)",
                     "Ambebi • შოუბიზნესი",
                     "საინტერესო ფაქტები და რჩევები",
                     "Formula • ფორმულა",
                     "ნიუს რუსთავი",
                     "სპორტი",
                     "gemrielia.ge • გემრიელია",
                     "მამა გაბრიელი",
                     "რეზონანსდეილი",
                     "ქართველები საბერძნეთში - georgians.gr",
                     "ერთიანი და ძლიერი ემიგრაცია",
                     "Барахолка город Батуми",
                     "რადიო თბილისი / FM 93.5 / Radio Tbilisi",
                     "TV Pirveli • ტელეკომპანია პირველი",
                     "ყიდვა - გაყიდვა - გაცვლა - გაქირავება",
                     "Spend 4 Seasons in Georgia",
                     "დავითხოვოთ პარლამენტი",
                     "ჩვენ ქართველები იტალიაში🇮🇹🇬🇪Noi georgiani in Italia",
                     "ქართველები ფილადელფიაში    GEORGIANS IN PHILADELPHIA",
                     "ბანკები და ფინანსები",
                     "საქართველოს ჯგუფი",
                     "Israelis georgian jews ישראלים יוצאי גאורגיה ქართველი ებრაელები ისრაელში",
                     "ტექნიკის ყიდვა-გაყიდვა • Teqnikis Yidva-Gayidva",
                     "Formula News • ფორმულა",
                     "ერთიანი ნაციონალური მოძრაობის მხარდამჭერები🇬🇪🇪🇺✌",
                     "ვაკანსია,დასაქმება",
                     "წმიდა მამათა სწავლანი 📖",
                     "საქართველო",
                     "GeoNews.ge",
                     "დასაქმება ვაკანსია საქართველოში/JOBS IN GEORGIA",
                     "instyle.ge",
                     "Front News - საქართველო")

cleaned_data <- cleaned_data[!(cleaned_data$`Page Name` %in% specified_pages), ]




 
#########################აქამდე

## remove unnecessary pages from data ____________ Phase 6
drop_pages<-as.data.frame(sort(table(cleaned_data$`Page Name`), decreasing = TRUE) [3:60])
drop_pages
write.xlsx(drop_pages,"C:/Users/crrc_/OneDrive/Documents/PHD thesis/PHD/PHD TSU/Thesis/data/Final data collection/drop unnecessary pages/drop_unnececarypages6.xlsx")
specified_pages <- c("ბათუმი - ყიდვა, გაყიდვა, გაქირავება / ყიდვა გაყიდვა გაქირავება ბათუმში",
                     "ყიდვა-გაყიდვა ბათუმში",
                     "Барахолка для Тбилисцев",
                     "Business Cafe - ბიზნეს კაფე",
                     "აკაკი წერეთლის სახელმწიფო უნივერსიტეტი /   Akaki Tsereteli State University",
                     "ავტომობილების ყიდვა - გაყიდვა საქართველოში",
                     "ედემი შოპი • Edemi Shop",
                     "Gogaggg",
                     "On.ge",
                     "სპორტი ყველასია",
                     "ბათუმელები • Batumelebi.ge",
                     "FashionHolics Online",
                     "ქართული გვერდი",
                     "IMEDINEWS",
                     "Недвижимость Грузии💸Real estate in Georgia💕უძრავი ქონება საქართველოში",
                     "POSTV - ახალი ამბები",
                     "იტალიის ცის ქვეშ – L'ANIMA GEORGIANA",
                     "ისტორია",
                     "უძრავი ქონების ყიდვა გაყიდვა, გაქირავება ,დაგირავება. udzravi qoneba",
                     "კვირის სიახლეები",
                     "დაიჯესტი • Digest",
                     "დევნილები დევნილებისთვის",
                     "უძრავი ქონების ყიდვა-გაყიდვა საქართველოში",
                     "პოზიტივი",
                     "ბარახოლკა-ბათუმი � � �BARAXOLKA---BATUMI",
                     "მალალუ - Malalu - ონლაინ შოპინგი",
                     "ემიგრანტის დღიურები ამერიკიდან(Immigrant's diaries from America)",
                     "Ambebi • შოუბიზნესი",
                     "საინტერესო ფაქტები და რჩევები",
                     "Formula • ფორმულა",
                     "ნიუს რუსთავი",
                     "სპორტი",
                     "gemrielia.ge • გემრიელია",
                     "მამა გაბრიელი",
                     "რეზონანსდეილი",
                     "ქართველები საბერძნეთში - georgians.gr",
                     "ერთიანი და ძლიერი ემიგრაცია",
                     "Барахолка город Батуми",
                     "რადიო თბილისი / FM 93.5 / Radio Tbilisi",
                     "TV Pirveli • ტელეკომპანია პირველი",
                     "ყიდვა - გაყიდვა - გაცვლა - გაქირავება",
                     "Spend 4 Seasons in Georgia",
                     "დავითხოვოთ პარლამენტი",
                     "ჩვენ ქართველები იტალიაში🇮🇹🇬🇪Noi georgiani in Italia",
                     "ქართველები ფილადელფიაში    GEORGIANS IN PHILADELPHIA",
                     "ბანკები და ფინანსები",
                     "საქართველოს ჯგუფი",
                     "Israelis georgian jews ישראלים יוצאי גאורגיה ქართველი ებრაელები ისრაელში",
                     "ტექნიკის ყიდვა-გაყიდვა • Teqnikis Yidva-Gayidva",
                     "Formula News • ფორმულა",
                     "ერთიანი ნაციონალური მოძრაობის მხარდამჭერები🇬🇪🇪🇺✌",
                     "ვაკანსია,დასაქმება",
                     "წმიდა მამათა სწავლანი 📖",
                     "საქართველო",
                     "GeoNews.ge",
                     "დასაქმება ვაკანსია საქართველოში/JOBS IN GEORGIA",
                     "instyle.ge",
                     "Front News - საქართველო")

cleaned_data <- cleaned_data[!(cleaned_data$`Page Name` %in% specified_pages), ]
write.xlsx(cleaned_data, "cleaned_data_quantiative analysis.xlsx")

