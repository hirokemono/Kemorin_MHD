!>@file   gz_write_field_labels.f90
!!@brief  module gz_write_field_labels
!!
!!@author H. Matsui
!!@date Programmed in June, 2009
!
!>@brief  Write field labels in one line
!!
!!@verbatim
!!      subroutine gz_write_one_label(FPz_f, label1, zbuf)
!!      subroutine gz_write_vector_label(FPz_f, label_v, zbuf)
!!      subroutine gz_write_sym_tensor_label(FPz_f, label_st, zbuf)
!!
!!      subroutine gz_write_two_labels(FPz_f, label1, label2, zbuf)
!!      subroutine gz_write_three_labels                                &
!!     &         (FPz_f, label1, label2, label3, zbuf)
!!      subroutine gz_write_four_labels                                 &
!!     &         (FPz_f, label1, label2, label3, label4, zbuf)
!!      subroutine gz_write_six_labels(FPz_f, label1, label2,           &
!!     &          label3, label4, label5, label6, zbuf)
!!      subroutine gz_write_seven_labels(FPz_f, label1, label2,         &
!!     &          label3, label4, label5, label6, label7, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_write_multi_labels(FPz_f, nlabel, labels, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!!
!
      module gz_write_field_labels
!
      use m_precision
      use m_constants
      use t_buffer_4_gzip
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_one_label(FPz_f, label1, zbuf)
!
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      character(len=kchara), intent(in) :: label1
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      call gz_write_chara_nolf(FPz_f, label1, zbuf)
!
      end subroutine gz_write_one_label
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_vector_label(FPz_f, label_v, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      character(len=kchara), intent(in) :: label_v(3)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call gz_write_multi_labels(FPz_f, ithree, label_v, zbuf)
!
      end subroutine gz_write_vector_label
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_sym_tensor_label(FPz_f, label_st, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      character(len=kchara), intent(in) :: label_st(6)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call gz_write_multi_labels(FPz_f, isix, label_st, zbuf)
!
      end subroutine gz_write_sym_tensor_label
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gz_write_two_labels(FPz_f, label1, label2, zbuf)
!
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      character(len=kchara), intent(in) :: label1, label2
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      call gz_write_chara_nolf(FPz_f, label1, zbuf)
      call gz_write_chara_nolf(FPz_f, label2, zbuf)
!
      end subroutine gz_write_two_labels
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_three_labels                                  &
     &         (FPz_f, label1, label2, label3, zbuf)
!
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      character(len=kchara), intent(in) :: label1, label2, label3
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      call gz_write_chara_nolf(FPz_f, label1, zbuf)
      call gz_write_chara_nolf(FPz_f, label2, zbuf)
      call gz_write_chara_nolf(FPz_f, label3, zbuf)
!
      end subroutine gz_write_three_labels
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_four_labels                                   &
     &         (FPz_f, label1, label2, label3, label4, zbuf)
!
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      character(len=kchara), intent(in) :: label1, label2
      character(len=kchara), intent(in) :: label3, label4
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      call gz_write_chara_nolf(FPz_f, label1, zbuf)
      call gz_write_chara_nolf(FPz_f, label2, zbuf)
      call gz_write_chara_nolf(FPz_f, label3, zbuf)
      call gz_write_chara_nolf(FPz_f, label4, zbuf)
!
      end subroutine gz_write_four_labels
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_six_labels(FPz_f, label1, label2,             &
     &          label3, label4, label5, label6, zbuf)
!
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      character(len=kchara), intent(in) :: label1, label2, label3
      character(len=kchara), intent(in) :: label4, label5, label6
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      call gz_write_chara_nolf(FPz_f, label1, zbuf)
      call gz_write_chara_nolf(FPz_f, label2, zbuf)
      call gz_write_chara_nolf(FPz_f, label3, zbuf)
      call gz_write_chara_nolf(FPz_f, label4, zbuf)
      call gz_write_chara_nolf(FPz_f, label5, zbuf)
      call gz_write_chara_nolf(FPz_f, label6, zbuf)
!
      end subroutine gz_write_six_labels
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_multi_labels(FPz_f, nlabel, labels, zbuf)
!
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: nlabel
      character(len=kchara), intent(in) :: labels(nlabel)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: nd
!
      do nd = 1, nlabel
        call gz_write_chara_nolf(FPz_f, labels(nd), zbuf)
      end do
!
      end subroutine gz_write_multi_labels
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gz_write_seven_labels(FPz_f, label1, label2,           &
     &          label3, label4, label5, label6, label7, zbuf)
!
      use gz_data_IO
!
      character, pointer, intent(in) :: FPz_f
      character(len=kchara), intent(in) :: label1, label2
      character(len=kchara), intent(in) :: label3, label4
      character(len=kchara), intent(in) :: label5, label6, label7
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      call gz_write_chara_nolf(FPz_f, label1, zbuf)
      call gz_write_chara_nolf(FPz_f, label2, zbuf)
      call gz_write_chara_nolf(FPz_f, label3, zbuf)
      call gz_write_chara_nolf(FPz_f, label4, zbuf)
      call gz_write_chara_nolf(FPz_f, label5, zbuf)
      call gz_write_chara_nolf(FPz_f, label6, zbuf)
      call gz_write_chara_nolf(FPz_f, label7, zbuf)
!
      end subroutine gz_write_seven_labels
!
! ----------------------------------------------------------------------
!
      end module gz_write_field_labels
