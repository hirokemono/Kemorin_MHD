!copy_sph_field_4_sph_trans.f90
!     module copy_sph_field_4_sph_trans
!
!>@file   copy_nodal_fields.f90
!!@brief  module copy_nodal_fields
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief  Copy field data (Need OMP PARALLEL)
!!
!!@verbatim
!!      subroutine copy_scalar_fld_from_trans(is_spec, i_trns)
!!      subroutine copy_scalar_fld_to_trans(is_spec, i_trns)
!!
!!      subroutine copy_vec_fld_from_trans(is_spec, i_trns)
!!      subroutine copy_vec_fld_to_trans(is_spec, i_trns)
!!
!!      subroutine copy_tensor_fld_from_trans(is_spec, i_trns)
!!      subroutine copy_tensor_fld_to_trans(is_spec, i_trns)
!!
!!      subroutine copy_scalar_from_MHD_trans(is_spec, i_trns)
!!      subroutine copy_vector_from_MHD_trans(is_spec, i_trns)
!!      subroutine copy_force_from_MHD_trans(is_spec, i_trns)
!!@endverbatim
!
!      Written by H. Matsui on Feb., 2008
!
      module copy_sph_field_4_sph_trans
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_sph_spectr_data
!
      use copy_field_smp
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_scalar_fld_from_trans(is_spec, i_trns)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_nod_scalar_smp(np_smp, nnod_rtp, inod_rtp_smp_stack,    &
     &    vr_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine copy_scalar_fld_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_fld_to_trans(is_spec, i_trns)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_nod_scalar_smp(np_smp, nnod_rtp, inod_rtp_smp_stack,    &
     &    d_rtp(1,is_spec), vr_rtp(1,i_trns))
!
      end subroutine copy_scalar_fld_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_vec_fld_from_trans(is_spec, i_trns)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_nod_vector_smp(np_smp, nnod_rtp, inod_rtp_smp_stack,    &
     &    vr_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine copy_vec_fld_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_vec_fld_to_trans(is_spec, i_trns)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_nod_vector_smp(np_smp, nnod_rtp, inod_rtp_smp_stack,    &
     &    d_rtp(1,is_spec), vr_rtp(1,i_trns))
!
      end subroutine copy_vec_fld_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_fld_from_trans(is_spec, i_trns)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_nod_sym_tensor_smp(np_smp, nnod_rtp,                    &
     &    inod_rtp_smp_stack, vr_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine copy_tensor_fld_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_fld_to_trans(is_spec, i_trns)
!
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_nod_sym_tensor_smp(np_smp, nnod_rtp,                    &
     &    inod_rtp_smp_stack, d_rtp(1,is_spec), vr_rtp(1,i_trns) )
!
      end subroutine copy_tensor_fld_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_from_MHD_trans(is_spec, i_trns)
!
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_nod_scalar_smp(np_smp, nnod_rtp, inod_rtp_smp_stack,    &
     &    fld_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine copy_scalar_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_MHD_trans(is_spec, i_trns)
!
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_nod_vector_smp(np_smp, nnod_rtp, inod_rtp_smp_stack,    &
     &    fld_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine copy_vector_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_force_from_MHD_trans(is_spec, i_trns)
!
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_nod_vector_smp(np_smp, nnod_rtp, inod_rtp_smp_stack,    &
     &    frc_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine copy_force_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      end module copy_sph_field_4_sph_trans
