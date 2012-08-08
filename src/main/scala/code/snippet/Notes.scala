package code.snippet

import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http.RequestVar
import reactive.BufferSignal
import code.model.Note
import reactive.web.Repeater
import net.liftweb.http.SHtml._
import net.liftweb.http.CssBoundLiftScreen
import net.liftweb.http.S
import net.liftweb.http.js.JsCmds._
import java.util.Date
import java.util.Calendar
import java.text.SimpleDateFormat

object Notes {

  val shortDateFormat = new SimpleDateFormat("yyyy.MM.dd HH:mm")

  object myNotes extends RequestVar[BufferSignal[Note]](BufferSignal[Note](Note.findAll :_*))

  def addNote(note: Note) = myNotes.is.value += note

  def list() = {
    "#tbl-notes-list" #> Repeater {
      myNotes.is.now map { note =>
        "@eachNote" #> {
          "@title *" #> note.title.is &
          "@addedDate *" #> shortTime(note.date.is) &
          "@edit [onclick]" #> ajaxInvoke(()=>{
            CreateNoteScreen.note(note)
            val js = CreateNoteScreen.rerender
            println("ReplayForm: "+js)
            Noop
          })
        }
      } signal
    }
  }

  def shortTime(cal: Calendar): String = {
    shortDateFormat.format(cal.getTime())
  }

}

object CreateNoteScreen extends CssBoundLiftScreen {

  object note extends ScreenVar(Note.createRecord.date(Calendar.getInstance()))

  override def defaultToAjax_? : Boolean = true

  val title: Field{type ValueType = String} = field(note.is.title)
  val noteField: Field{type ValueType = String} = field(note.is.note)

  // Validation
  def formName = "noteFormContent"

  def finish() {
    note.is.save(true)
    S.notice("Created New Note.")
    Notes.addNote(note)
    AjaxOnDone.set(Noop)
//    AjaxOnDone.set(SetHtml("div-feedback-form-result", <b>All done!</b>))
    //AjaxOnDone.set(Run("$('.field-error', '#id-form-contact').hide()"))
  }

  def rerender = {
    println("Value of ScreenVar: "+note.is)
    this.replayForm
  }

}