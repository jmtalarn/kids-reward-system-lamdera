module I18n exposing (..)


import Types exposing (Language(..))
import Time exposing (Weekday(..))

appTitle : Language -> String
appTitle lang =
    case lang of
        En ->
            "Kids Reward System"

        Es ->
            "Puntos y Recompensas"

        Ca ->
            "Punts i Recompenses"


welcomeBack : Language -> String
welcomeBack lang =
    case lang of
        En ->
            "Welcome back,"

        Es ->
            "¡Hola de nuevo,"

        Ca ->
            "Hola de nou,"


pointsLabel : Language -> String
pointsLabel lang =
    case lang of
        En ->
            "Points"

        Es ->
            "Puntos"

        Ca ->
            "Punts"


tabLabels : Language -> { missions : String, calendar : String, shop : String, activity : String, parent : String }
tabLabels lang =
    case lang of
        En ->
            { missions = "Missions", calendar = "Calendar", shop = "Shop", activity = "Activity", parent = "Parent" }

        Es ->
            { missions = "Misiones", calendar = "Calendario", shop = "Tienda", activity = "Actividad", parent = "Padres" }

        Ca ->
            { missions = "Missions", calendar = "Calendari", shop = "Botiga", activity = "Activitat", parent = "Pares" }


redeemNow : Language -> String
redeemNow lang =
    case lang of
        En ->
            "Redeem Now"

        Es ->
            "Canjear"

        Ca ->
            "Bescanviar"


lockedLabel : Language -> String
lockedLabel lang =
    case lang of
        En ->
            "Locked"

        Es ->
            "Bloqueado"

        Ca ->
            "Bloquejat"


readyToRedeem : Language -> String
readyToRedeem lang =
    case lang of
        En ->
            "🎉 Ready to redeem!"

        Es ->
            "🎉 ¡Listo para canjear!"

        Ca ->
            "🎉 Llest per bescanviar!"


morePtsNeeded : Language -> Int -> Int -> String
morePtsNeeded lang pts pct =
    case lang of
        En ->
            String.fromInt pts ++ " more pts needed (" ++ String.fromInt pct ++ "%)"

        Es ->
            "Faltan " ++ String.fromInt pts ++ " pts (" ++ String.fromInt pct ++ "%)"

        Ca ->
            "Falten " ++ String.fromInt pts ++ " pts (" ++ String.fromInt pct ++ "%)"


prevWeek : Language -> String
prevWeek lang =
    case lang of
        En ->
            "← Prev Week"

        Es ->
            "← Semana Anterior"

        Ca ->
            "← Setmana Anterior"


nextWeek : Language -> String
nextWeek lang =
    case lang of
        En ->
            "Next Week →"

        Es ->
            "Próxima Semana →"

        Ca ->
            "Pròxima Setmana →"


today : Language -> String
today lang =
    case lang of
        En ->
            "Today"

        Es ->
            "Hoy"

        Ca ->
            "Avui"


tasksForLabel : Language -> String
tasksForLabel lang =
    case lang of
        En ->
            "📋 Tasks for"

        Es ->
            "📋 Tareas para"

        Ca ->
            "📋 Tasques per a"


rewardShop : Language -> String
rewardShop lang =
    case lang of
        En ->
            "🎁 Reward Shop"

        Es ->
            "🎁 Tienda de Premios"

        Ca ->
            "🎁 Botiga de Premis"


recentActivity : Language -> String
recentActivity lang =
    case lang of
        En ->
            "📜 Recent Activity"

        Es ->
            "📜 Actividad Reciente"

        Ca ->
            "📜 Activitat Recent"


noActivityYet : Language -> String
noActivityYet lang =
    case lang of
        En ->
            "No activity yet."

        Es ->
            "Aún no hay actividad."

        Ca ->
            "Sense activitat encara."


assignToEveryone : Language -> String
assignToEveryone lang =
    case lang of
        En ->
            "Assign to: Everyone"

        Es ->
            "Asignar a: Todos"

        Ca ->
            "Assignar a: Tots"


assignTo : Language -> String
assignTo lang =
    case lang of
        En ->
            "Assign to:"

        Es ->
            "Asignar a:"

        Ca ->
            "Assignar a:"


applyButton : Language -> String
applyButton lang =
    case lang of
        En ->
            "Apply"

        Es ->
            "Aplicar"

        Ca ->
            "Aplicar"


dailyLabel : Language -> String
dailyLabel lang =
    case lang of
        En ->
            "Daily"

        Es ->
            "Diaria"

        Ca ->
            "Diària"


weeklyLabel : Language -> String
weeklyLabel lang =
    case lang of
        En ->
            "Weekly"

        Es ->
            "Semanal"

        Ca ->
            "Setmanal"


monthlyLabel : Language -> String
monthlyLabel lang =
    case lang of
        En ->
            "Monthly"

        Es ->
            "Mensual"

        Ca ->
            "Mensual"


missionsAndChores : Language -> String
missionsAndChores lang =
    case lang of
        En ->
            "🎯 Missions & Chores"

        Es ->
            "🎯 Misiones y Tareas"

        Ca ->
            "🎯 Missions i Tasques"


selectOrAddKid : Language -> String
selectOrAddKid lang =
    case lang of
        En ->
            "Select or add a kid first."

        Es ->
            "Selecciona o agrega un niño primero."

        Ca ->
            "Selecciona o afegeix un nen primer."


unlockParentMode : Language -> String
unlockParentMode lang =
    case lang of
        En ->
            "🔒 Unlock Parent Mode at the top to manage kids, tasks, and rewards."

        Es ->
            "🔒 Desbloquea Modo Padres en la parte superior para gestionar niños, tareas y premios."

        Ca ->
            "🔒 Desbloqueja el Mode Pares a la part superior per gestionar nens, tasques i premis."


tasksSection : Language -> String
tasksSection lang =
    case lang of
        En ->
            "Tasks"

        Es ->
            "Tareas"

        Ca ->
            "Tasques"


rewardShopSection : Language -> String
rewardShopSection lang =
    case lang of
        En ->
            "Reward Shop"

        Es ->
            "Tienda de Premios"

        Ca ->
            "Botiga de Premis"


languageLabel : Language -> String
languageLabel lang =
    case lang of
        En ->
            "🌍 Idioma / Language"

        Es ->
            "🌍 Idioma / Language"

        Ca ->
            "🌍 Idioma / Language"


chooseLanguage : Language -> String
chooseLanguage lang =
    case lang of
        En ->
            "Choose display language for kids"

        Es ->
            "Elige el idioma de visualización para niños"

        Ca ->
            "Tria l'idioma de visualització per als nens"


addButton : Language -> String
addButton lang =
    case lang of
        En ->
            "+ Add"

        Es ->
            "+ Añadir"

        Ca ->
            "+ Afegir"


specificDays : Language -> String
specificDays lang =
    case lang of
        En ->
            "Specific Days"

        Es ->
            "Días Específicos"

        Ca ->
            "Dies Específics"


oneTime : Language -> String
oneTime lang =
    case lang of
        En ->
            "One-Time"

        Es ->
            "Una Vez"

        Ca ->
            "Un Cop"


monFri : Language -> String
monFri lang =
    case lang of
        En ->
            "Mon–Fri"

        Es ->
            "Lun–Vie"

        Ca ->
            "Dl–Dv"


satSun : Language -> String
satSun lang =
    case lang of
        En ->
            "Sat–Sun"

        Es ->
            "Sáb–Dom"

        Ca ->
            "Ds–Dg"


cancelButton : Language -> String
cancelButton lang =
    case lang of
        En ->
            "Cancel"

        Es ->
            "Cancelar"

        Ca ->
            "Cancelar"


editButton : Language -> String
editButton lang =
    case lang of
        En ->
            "✏️ Edit"

        Es ->
            "✏️ Editar"

        Ca ->
            "✏️ Editar"


editTaskTitle : Language -> String
editTaskTitle lang =
    case lang of
        En ->
            "Edit Mission / Task"

        Es ->
            "Editar Misión / Tarea"

        Ca ->
            "Editar Missió / Tasca"


createTaskTitle : Language -> String
createTaskTitle lang =
    case lang of
        En ->
            "Create Mission / Task"

        Es ->
            "Crear Misión / Tarea"

        Ca ->
            "Crear Missió / Tasca"


editRewardTitle : Language -> String
editRewardTitle lang =
    case lang of
        En ->
            "Edit Shop Reward"

        Es ->
            "Editar Premio de Tienda"

        Ca ->
            "Editar Premi de Botiga"


createRewardTitle : Language -> String
createRewardTitle lang =
    case lang of
        En ->
            "Create Shop Reward"

        Es ->
            "Crear Premio de Tienda"

        Ca ->
            "Crear Premi de Botiga"


saveButton : Language -> String
saveButton lang =
    case lang of
        En ->
            "Save"

        Es ->
            "Guardar"

        Ca ->
            "Guardar"


archiveButtonText : Language -> String
archiveButtonText lang =
    case lang of
        En ->
            "Archive"

        Es ->
            "Archivar"

        Ca ->
            "Arxivar"


restoreButtonText : Language -> String
restoreButtonText lang =
    case lang of
        En ->
            "Restore"

        Es ->
            "Restaurar"

        Ca ->
            "Restaurar"


parentPinTitle : Language -> String
parentPinTitle lang =
    case lang of
        En ->
            "Parent PIN"

        Es ->
            "PIN de Padres"

        Ca ->
            "PIN de Pares"


pinInstructions : Language -> String
pinInstructions lang =
    case lang of
        En ->
            "Enter code & tap ✓"

        Es ->
            "Ingrese código y presione ✓"

        Ca ->
            "Introduïu codi i premeu ✓"


unlockButton : Language -> String
unlockButton lang =
    case lang of
        En ->
            "✓ Unlock"

        Es ->
            "✓ Desbloquear"

        Ca ->
            "✓ Desbloquear"


taskTitlePlaceholder : Language -> String
taskTitlePlaceholder lang =
    case lang of
        En ->
            "Task title"

        Es ->
            "Título de la tarea"

        Ca ->
            "Títol de la tasca"


ptsPlaceholder : Language -> String
ptsPlaceholder lang =
    case lang of
        En ->
            "Pts"

        Es ->
            "Pts"

        Ca ->
            "Pts"


bonusReasonPlaceholder : Language -> String
bonusReasonPlaceholder lang =
    case lang of
        En ->
            "Reason (e.g. Great teamwork)"

        Es ->
            "Razón (ej. Gran trabajo en equipo)"

        Ca ->
            "Motiu (p.ex. Excel·lent treball en equip)"


rewardTitlePlaceholder : Language -> String
rewardTitlePlaceholder lang =
    case lang of
        En ->
            "Reward title"

        Es ->
            "Título del premio"

        Ca ->
            "Títol del premi"


costPlaceholder : Language -> String
costPlaceholder lang =
    case lang of
        En ->
            "Cost"

        Es ->
            "Costo"

        Ca ->
            "Cost"


quickAdjustmentTitle : Language -> String -> String -> String
quickAdjustmentTitle lang avatar name =
    case lang of
        En ->
            "Quick Adjustment for " ++ avatar ++ " " ++ name

        Es ->
            "Ajuste Rápido para " ++ avatar ++ " " ++ name

        Ca ->
            "Ajust Ràpid per a " ++ avatar ++ " " ++ name


manageSectionTitle : Language -> String
manageSectionTitle lang =
    case lang of
        En ->
            "Manage Kids, Tasks & Rewards"

        Es ->
            "Gestionar Niños, Tareas y Premios"

        Ca ->
            "Gestionar Nens, Tasques i Premis"


deleteButton : Language -> String
deleteButton lang =
    case lang of
        En ->
            "🗑️"

        Es ->
            "🗑️"

        Ca ->
            "🗑️"


kidNamePlaceholder : Language -> String
kidNamePlaceholder lang =
    case lang of
        En ->
            "Kid name"

        Es ->
            "Nombre del niño"

        Ca ->
            "Nom del nen"


bonusAmountPlaceholder : Language -> String
bonusAmountPlaceholder lang =
    case lang of
        En ->
            "+10 / -5"

        Es ->
            "+10 / -5"

        Ca ->
            "+10 / -5"


weekdays : Language -> List String
weekdays lang =
    case lang of
        En ->
            [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]

        Es ->
            [ "Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom" ]

        Ca ->
            [ "Dl", "Dt", "Dc", "Dj", "Dv", "Ds", "Dg" ]


shortDayName : Language -> Time.Weekday -> String
shortDayName lang day =
    let
        idx =
            case day of
                Time.Mon ->
                    0

                Time.Tue ->
                    1

                Time.Wed ->
                    2

                Time.Thu ->
                    3

                Time.Fri ->
                    4

                Time.Sat ->
                    5

                Time.Sun ->
                    6
    in
    weekdays lang
        |> List.drop idx
        |> List.head
        |> Maybe.withDefault ""


pendingLabel : Language -> String
pendingLabel lang =
    case lang of
        En ->
            "⏳ Pending"

        Es ->
            "⏳ Pendiente"

        Ca ->
            "⏳ Pendent"


approveButton : Language -> String
approveButton lang =
    case lang of
        En ->
            "✅ Approve"

        Es ->
            "✅ Aprobar"

        Ca ->
            "✅ Aprovar"


rejectButton : Language -> String
rejectButton lang =
    case lang of
        En ->
            "❌ Reject"

        Es ->
            "❌ Rechazar"

        Ca ->
            "❌ Rebutjar"


chooseEmoji : Language -> String
chooseEmoji lang =
    case lang of
        En ->
            "Choose an Emoji"

        Es ->
            "Elige un Emoji"

        Ca ->
            "Tria un Emoji"

streakLabel : Language -> Int -> String
streakLabel lang days =
    case lang of
        En ->
            if days == 1 then
                "🔥 1-day streak"

            else
                "🔥 " ++ String.fromInt days ++ "-day streak"

        Es ->
            if days == 1 then
                "🔥 Racha de 1 día"

            else
                "🔥 Racha de " ++ String.fromInt days ++ " días"

        Ca ->
            if days == 1 then
                "🔥 Ratxa d'1 dia"

            else
                "🔥 Ratxa de " ++ String.fromInt days ++ " dies"


startDateLabel : Language -> String
startDateLabel lang =
    case lang of
        En ->
            "Start:"

        Es ->
            "Inicio:"

        Ca ->
            "Inici:"


endDateLabel : Language -> String
endDateLabel lang =
    case lang of
        En ->
            "End:"

        Es ->
            "Fin:"

        Ca ->
            "Fi:"


noKidsMessage : Language -> String
noKidsMessage lang =
    case lang of
        En ->
            "No kids yet. Unlock Parent Mode to add one!"

        Es ->
            "Sin niños aún. ¡Desbloquea Modo Padres para agregar uno!"

        Ca ->
            "Sense nens encara. Desbloqueja el Mode Pares per afegir un!"


lockButtonLabel : Language -> String
lockButtonLabel lang =
    case lang of
        En ->
            "🔒 Lock"

        Es ->
            "🔒 Bloquear"

        Ca ->
            "🔒 Bloquejar"


parentButtonLabel : Language -> String
parentButtonLabel lang =
    case lang of
        En ->
            "🔐 Parent"

        Es ->
            "🔐 Padres"

        Ca ->
            "🔐 Pares"


addKidButton : Language -> String
addKidButton lang =
    case lang of
        En ->
            "Add Kid"

        Es ->
            "Añadir Niño"

        Ca ->
            "Afegir Nen"


editKidButton : Language -> String
editKidButton lang =
    case lang of
        En ->
            "Edit Kid"

        Es ->
            "Editar Niño"

        Ca ->
            "Editar Nen"


instantPointsLabel : Language -> String
instantPointsLabel lang =
    case lang of
        En ->
            "⚡ Instant Points (No Approval Needed)"

        Es ->
            "⚡ Puntos Instantáneos (Sin Aprobación)"

        Ca ->
            "⚡ Punts Instantanis (Sense Aprovació)"


requiresApprovalLabel : Language -> String
requiresApprovalLabel lang =
    case lang of
        En ->
            "⏳ Requires Parent Approval: ON"

        Es ->
            "⏳ Requiere Aprobación de Padres: ON"

        Ca ->
            "⏳ Requereix Aprovació dels Pares: ON"


dailyRecurrence : Language -> String
dailyRecurrence lang =
    case lang of
        En ->
            "Daily"

        Es ->
            "Diaria"

        Ca ->
            "Diària"


weeklyRecurrence : Language -> String
weeklyRecurrence lang =
    case lang of
        En ->
            "Weekly"

        Es ->
            "Semanal"

        Ca ->
            "Setmanal"


monthlyRecurrence : Language -> String
monthlyRecurrence lang =
    case lang of
        En ->
            "Monthly"

        Es ->
            "Mensual"

        Ca ->
            "Mensual"


oneTimeRecurrence : Language -> String
oneTimeRecurrence lang =
    case lang of
        En ->
            "OneTime"

        Es ->
            "Una Vez"

        Ca ->
            "Un Cop"
