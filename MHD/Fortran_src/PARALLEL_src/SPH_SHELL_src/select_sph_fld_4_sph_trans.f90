!>@file   select_sph_fld_4_sph_trans.f90
!!@brief  module select_sph_fld_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief  Copy field data (Need OMP PARALLEL)
!!
!!@verbatim
!!      subroutine sel_scalar_fld_from_trans(is_spec, i_trns)
!!      subroutine sel_vector_fld_from_trans(is_spec, i_trns)
!!      subroutine sel_tensor_fld_from_trans(is_spec, i_trns)
!!
!!      subroutine sel_scalar_fld_to_trans(is_spec, i_trns)
!!      subroutine sel_vector_fld_to_trans(is_spec, i_trns)
!!      subroutine sel_tensor_fld_to_trans(is_spec, i_trns)
!!@endverbatim
!
!      Written by H. Matsui on Feb., 2008
!
      module select_sph_fld_4_sph_trans
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_sph_spectr_data
!
      use copy_field_4_sph_trans
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine sel_scalar_fld_from_trans(is_spec, i_trns)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_scalar_from_trans                                       &
     &    (nnod_rtp, vr_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine sel_scalar_fld_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine sel_vector_fld_from_trans(is_spec, i_trns)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_vector_from_trans                                       &
     &    (nnod_rtp, vr_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine sel_vector_fld_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine sel_tensor_fld_from_trans(is_spec, i_trns)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_tensor_from_trans                                       &
     &    (nnod_rtp, vr_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine sel_tensor_fld_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_scalar_fld_to_trans(is_spec, i_trns)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_scalar_to_trans                                         &
     &    (nnod_rtp, d_rtp(1,is_spec), vr_rtp(1,i_trns) )
!
      end subroutine sel_scalar_fld_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine sel_vector_fld_to_trans(is_spec, i_trns)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_vector_to_trans                                         &
     &    (nnod_rtp, d_rtp(1,is_spec), vr_rtp(1,i_trns) )
!
      end subroutine sel_vector_fld_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine sel_tensor_fld_to_trans(is_spec, i_trns)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_tensor_to_trans                                         &
     &    (nnod_rtp, d_rtp(1,is_spec), vr_rtp(1,i_trns) )
!
      end subroutine sel_tensor_fld_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      end module select_sph_fld_4_sph_trans
