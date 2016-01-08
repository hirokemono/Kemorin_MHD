!cal_w_filtering_scalars.f90
!      module cal_w_filtering_scalars
!
!      Written by H. Matsui on nov., 2008
!
!!      subroutine cal_w_filtered_scalar(i_filter, i_scalar,            &
!!     &          nod_comm, node, nod_fld)
!!      subroutine cal_w_filtered_vector(i_filter, i_vect,              &
!!     &          nod_comm, node, nod_fld)
!!      subroutine cal_w_filtered_sym_tensor(i_filter, i_vect,          &
!!     &          nod_comm, node, nod_fld)
!!
!!      subroutine cal_w_filtered_scalar_in_fluid(i_filter, i_scalar,   &
!!     &          nod_comm, node, nod_fld)
!!      subroutine cal_w_filtered_vector_in_fluid(i_filter, i_vect,     &
!!     &          nod_comm, node, nod_fld)
!!      subroutine cal_w_filtered_tensor_in_fluid(i_filter, i_vect,     &
!!     &          nod_comm, node, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(inout) :: nod_fld
!!          i_filter: field ID foe filtered field
!!          i_scalar: original field ID
!
      module cal_w_filtering_scalars
!
      use m_precision
!
      use m_control_parameter
!
      use t_comm_table
      use t_geometry_data
      use t_phys_data
!
      use cal_w_filter_phys
      use cal_w_filter_phys_smp
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_filtered_scalar(i_filter, i_scalar,              &
     &          nod_comm, node, nod_fld)
!
!
      integer (kind=kint), intent(in) :: i_filter, i_scalar
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING) then
!
        call cal_w_ez_filter_scalar_phys(nod_comm, node,                &
     &      num_whole_w_filter_grp, id_whole_w_filter_grp, i_scalar,    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_w_filter_scalar_phys_smp(nod_comm, node,               &
     &      num_whole_w_filter_grp, id_whole_w_filter_grp, i_scalar,    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_w_ez_filter_scalar_smp(nod_comm, node,                 &
     &      num_whole_w_filter_grp, id_whole_w_filter_grp, i_scalar,    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_w_filter_scalar_phys(nod_comm, node,                   &
     &      num_whole_w_filter_grp, id_whole_w_filter_grp, i_scalar,    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
      end if
!
      end subroutine cal_w_filtered_scalar
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_filtered_vector(i_filter, i_vect,                &
     &          nod_comm, node, nod_fld)
!
      integer (kind=kint), intent(in) :: i_filter, i_vect
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING ) then
        call cal_w_ez_filter_vector_phys                                &
     &     (nod_comm, node, num_whole_filter_grp, id_whole_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_w_filter_vector_phys_smp                               &
     &     (nod_comm, node, num_whole_filter_grp, id_whole_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_w_ez_filter_vector_smp                                 &
     &     (nod_comm, node, num_whole_filter_grp, id_whole_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_w_filter_vector_phys                                   &
     &     (nod_comm, node, num_whole_filter_grp, id_whole_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
      end if
!
      end subroutine cal_w_filtered_vector
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_filtered_sym_tensor(i_filter, i_vect,            &
     &          nod_comm, node, nod_fld)
!
      integer (kind = kint), intent(in) :: i_filter, i_vect
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING ) then
!
        call cal_w_ez_filter_tensor_phys                                &
     &     (nod_comm, node, num_whole_filter_grp, id_whole_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_w_filter_tensor_phys_smp                               &
     &     (nod_comm, node, num_whole_filter_grp, id_whole_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_w_ez_filter_tensor_smp                                 &
     &     (nod_comm, node, num_whole_filter_grp, id_whole_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_w_filter_tensor_phys                                   &
     &     (nod_comm, node, num_whole_filter_grp, id_whole_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
      end if
!
      end subroutine cal_w_filtered_sym_tensor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_w_filtered_scalar_in_fluid(i_filter, i_scalar,     &
     &          nod_comm, node, nod_fld)
!
      integer (kind=kint), intent(in) :: i_filter, i_scalar
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING) then
!
        call cal_w_ez_filter_scalar_phys(nod_comm, node,                &
     &      num_fluid_w_filter_grp, id_fluid_w_filter_grp, i_scalar,    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_w_filter_scalar_phys_smp(nod_comm, node,               &
     &      num_fluid_w_filter_grp, id_fluid_w_filter_grp, i_scalar,    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_w_ez_filter_scalar_smp(nod_comm, node,                 &
     &      num_fluid_w_filter_grp, id_fluid_w_filter_grp, i_scalar,    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_w_filter_scalar_phys(nod_comm, node,                   &
     &      num_fluid_w_filter_grp, id_fluid_w_filter_grp, i_scalar,    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
      end if
!
      end subroutine cal_w_filtered_scalar_in_fluid
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_filtered_vector_in_fluid(i_filter, i_vect,       &
     &          nod_comm, node, nod_fld)
!
      integer (kind=kint), intent(in) :: i_filter, i_vect
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING ) then
        call cal_w_ez_filter_vector_phys                                &
     &     (nod_comm, node, num_fluid_filter_grp, id_fluid_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_w_filter_vector_phys_smp                               &
     &     (nod_comm, node, num_fluid_filter_grp, id_fluid_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_w_ez_filter_vector_smp                                 &
     &     (nod_comm, node, num_fluid_filter_grp, id_fluid_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_w_filter_vector_phys                                   &
     &     (nod_comm, node, num_fluid_filter_grp, id_fluid_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
      end if
!
      end subroutine cal_w_filtered_vector_in_fluid
!
! ----------------------------------------------------------------------
!
      subroutine cal_w_filtered_tensor_in_fluid(i_filter, i_vect,       &
     &          nod_comm, node, nod_fld)
!
      integer (kind=kint), intent(in) :: i_filter, i_vect
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING ) then
!
        call cal_w_ez_filter_tensor_phys                                &
     &     (nod_comm, node, num_fluid_filter_grp, id_fluid_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_w_filter_tensor_phys_smp                               &
     &     (nod_comm, node, num_fluid_filter_grp, id_fluid_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_w_ez_filter_tensor_smp                                 &
     &     (nod_comm, node, num_fluid_filter_grp, id_fluid_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_w_filter_tensor_phys                                   &
     &     (nod_comm, node, num_fluid_filter_grp, id_fluid_filter_grp,  &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
      end if
!
      end subroutine cal_w_filtered_tensor_in_fluid
!
! ----------------------------------------------------------------------
!
      end module cal_w_filtering_scalars
