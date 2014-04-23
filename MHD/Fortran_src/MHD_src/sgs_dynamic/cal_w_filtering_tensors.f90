!
!      module cal_w_filtering_tensors
!
!      Written by H. Matsui
!
!      subroutine cal_w_filtered_sym_tensor(i_filter, i_vect)
!      subroutine cal_w_filtered_tensor_in_fluid(i_filter, i_vect)
!          i_filter: field UD foe filtered field
!          i_vect:   original field ID
!
      module cal_w_filtering_tensors
!
      use m_precision
!
      use m_control_parameter
!
      use cal_w_filter_phys
      use cal_w_filter_phys_smp
!
      implicit none
!
      private :: cal_w_filtered_tensor_in_fluid
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_filtered_sym_tensor(i_filter, i_vect)
!
       integer (kind = kint), intent(in) :: i_filter, i_vect
!
!
      if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING ) then
!
        call cal_w_ez_filter_tensor_phys(num_whole_filter_grp,          &
     &      id_whole_filter_grp, i_filter, i_vect)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_w_filter_tensor_phys_smp(num_whole_filter_grp,         &
     &      id_whole_filter_grp, i_filter, i_vect)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_w_ez_filter_tensor_smp(num_whole_filter_grp,           &
     &      id_whole_filter_grp, i_filter, i_vect)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_w_filter_tensor_phys(num_whole_filter_grp,             &
     &      id_whole_filter_grp, i_filter, i_vect)
!
      end if
!
      end subroutine cal_w_filtered_sym_tensor
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_filtered_tensor_in_fluid(i_filter, i_vect)
!
       integer (kind=kint), intent(in) :: i_filter, i_vect
!
!
      if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING ) then
!
        call cal_w_ez_filter_tensor_phys(num_fluid_filter_grp,          &
     &      id_fluid_filter_grp, i_filter, i_vect)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_w_filter_tensor_phys_smp(num_fluid_filter_grp,         &
     &      id_fluid_filter_grp, i_filter, i_vect)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_w_ez_filter_tensor_smp(num_fluid_filter_grp,           &
     &      id_fluid_filter_grp, i_filter, i_vect)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_w_filter_tensor_phys(num_fluid_filter_grp,             &
     &      id_fluid_filter_grp, i_filter, i_vect)
!
      end if
!
      end subroutine cal_w_filtered_tensor_in_fluid
!
! ----------------------------------------------------------------------
!
      end module cal_w_filtering_tensors
