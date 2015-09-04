!cal_w_filtering_scalars.f90
!      module cal_w_filtering_scalars
!
!      Written by H. Matsui on nov., 2008
!
!      subroutine cal_w_filtered_scalar(n_filter, i_filter, i_scalar)
!      subroutine cal_w_filtered_scalar_in_fluid(i_filter, i_scalar)
!          i_filter: field ID foe filtered field
!          i_scalar: original field ID
!
      module cal_w_filtering_scalars
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
      private :: cal_w_filtered_scalar_in_fluid
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_filtered_scalar(i_filter, i_scalar)
!
      use m_node_phys_data
!
      integer (kind=kint), intent(in) :: i_filter, i_scalar
!
!
      if (iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING) then
!
        call cal_w_ez_filter_scalar_phys                                &
     &     (num_whole_w_filter_grp, id_whole_w_filter_grp, i_scalar,    &
     &      num_tot_nod_phys, i_filter, d_nod)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_w_filter_scalar_phys_smp                               &
     &     (num_whole_w_filter_grp, id_whole_w_filter_grp, i_scalar,    &
     &      num_tot_nod_phys, i_filter, d_nod)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_w_ez_filter_scalar_smp                                 &
     &     (num_whole_w_filter_grp, id_whole_w_filter_grp, i_scalar,    &
     &      num_tot_nod_phys, i_filter, d_nod)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_w_filter_scalar_phys                                   &
     &     (num_whole_w_filter_grp, id_whole_w_filter_grp, i_scalar,    &
     &      num_tot_nod_phys, i_filter, d_nod)
      end if
!
      end subroutine cal_w_filtered_scalar
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_filtered_scalar_in_fluid(i_filter, i_scalar)
!
      use m_node_phys_data
!
      integer (kind=kint), intent(in) :: i_filter, i_scalar
!
!
      if (iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING) then
!
        call cal_w_ez_filter_scalar_phys                                &
     &     (num_fluid_w_filter_grp, id_fluid_w_filter_grp, i_scalar,    &
     &      num_tot_nod_phys, i_filter, d_nod)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_w_filter_scalar_phys_smp                               &
     &     (num_fluid_w_filter_grp, id_fluid_w_filter_grp, i_scalar,    &
     &      num_tot_nod_phys, i_filter, d_nod)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_w_ez_filter_scalar_smp                                 &
     &     (num_fluid_w_filter_grp, id_fluid_w_filter_grp, i_scalar,    &
     &      num_tot_nod_phys, i_filter, d_nod)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_w_filter_scalar_phys                                   &
     &     (num_fluid_w_filter_grp, id_fluid_w_filter_grp, i_scalar,    &
     &      num_tot_nod_phys, i_filter, d_nod)
      end if
!
      end subroutine cal_w_filtered_scalar_in_fluid
!
! ----------------------------------------------------------------------
!
      end module cal_w_filtering_scalars
