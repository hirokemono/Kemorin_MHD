!>@file   gz_write_field_labels.f90
!!@brief  module gz_write_field_labels
!!
!!@author H. Matsui
!!@date Programmed in June, 2009
!
!>@brief  Write field labels in one line
!!
!!@verbatim
!!      subroutine gz_write_one_label(label1)
!!      subroutine gz_write_vector_label(label_v)
!!      subroutine gz_write_sym_tensor_label(label_st)
!!
!!      subroutine gz_write_two_labels(label1, label2)
!!      subroutine gz_write_three_labels(label1, label2, label3)
!!      subroutine gz_write_four_labels(label1, label2,           &
!!     &          label3, label4)
!!      subroutine gz_write_six_labels(label1, label2,            &
!!     &          label3, label4, label5, label6)
!!      subroutine gz_write_seven_labels(label1, label2,          &
!!     &          label3, label4, label5, label6, label7)
!!
!!      subroutine gz_write_multi_labels(nlabel, labels)
!!@endverbatim
!!
!
      module gz_write_field_labels
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      private :: gz_write_one_label_cont
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_one_label(label1)
!
      character(len=kchara), intent(in) :: label1
!
      call gz_write_one_label_cont(label1)
!
      end subroutine gz_write_one_label
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_vector_label(label_v)
!
      character(len=kchara), intent(in) :: label_v(3)
!
!
      call gz_write_multi_labels(ithree, label_v)
!
      end subroutine gz_write_vector_label
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_sym_tensor_label(label_st)
!
      character(len=kchara), intent(in) :: label_st(6)
!
!
      call gz_write_multi_labels(isix, label_st)
!
      end subroutine gz_write_sym_tensor_label
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gz_write_two_labels(label1, label2)
!
      character(len=kchara), intent(in) :: label1, label2
!
      call gz_write_one_label_cont(label1)
      call gz_write_one_label_cont(label2)
!
      end subroutine gz_write_two_labels
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_three_labels(label1, label2, label3)
!
      character(len=kchara), intent(in) :: label1, label2, label3
!
      call gz_write_one_label_cont(label1)
      call gz_write_one_label_cont(label2)
      call gz_write_one_label_cont(label3)
!
      end subroutine gz_write_three_labels
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_four_labels(label1, label2,                   &
     &          label3, label4)
!
      character(len=kchara), intent(in) :: label1, label2
      character(len=kchara), intent(in) :: label3, label4
!
      call gz_write_one_label_cont(label1)
      call gz_write_one_label_cont(label2)
      call gz_write_one_label_cont(label3)
      call gz_write_one_label_cont(label4)
!
      end subroutine gz_write_four_labels
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_six_labels(label1, label2,                    &
     &          label3, label4, label5, label6)
!
      character(len=kchara), intent(in) :: label1, label2, label3
      character(len=kchara), intent(in) :: label4, label5, label6
!
      call gz_write_one_label_cont(label1)
      call gz_write_one_label_cont(label2)
      call gz_write_one_label_cont(label3)
      call gz_write_one_label_cont(label4)
      call gz_write_one_label_cont(label5)
      call gz_write_one_label_cont(label6)
!
      end subroutine gz_write_six_labels
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_multi_labels(nlabel, labels)
!
      integer(kind = kint), intent(in) :: nlabel
      character(len=kchara), intent(in) :: labels(nlabel)
!
      integer(kind = kint) :: nd
!
      do nd = 1, nlabel
        call gz_write_one_label_cont(labels(nd) )
      end do
!
      end subroutine gz_write_multi_labels
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gz_write_seven_labels(label1, label2,                  &
     &          label3, label4, label5, label6, label7)
!
      character(len=kchara), intent(in) :: label1, label2
      character(len=kchara), intent(in) :: label3, label4
      character(len=kchara), intent(in) :: label5, label6, label7
!
      call gz_write_one_label_cont(label1)
      call gz_write_one_label_cont(label2)
      call gz_write_one_label_cont(label3)
      call gz_write_one_label_cont(label4)
      call gz_write_one_label_cont(label5)
      call gz_write_one_label_cont(label6)
      call gz_write_one_label_cont(label7)
!
      end subroutine gz_write_seven_labels
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gz_write_one_label_cont(label1)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: label1
!
      write(textbuf,'(2a,a1)') trim(label1), '    ', CHAR(0)
      call write_compress_txt_nolf(nbuf, textbuf)
!
      end subroutine gz_write_one_label_cont
!
! ----------------------------------------------------------------------
!
      end module gz_write_field_labels
