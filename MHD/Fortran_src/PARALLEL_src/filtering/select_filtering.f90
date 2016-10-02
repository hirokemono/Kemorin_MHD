!
!      module select_filtering
!
!      Written by H. Matsui
!
!!      subroutine cal_filtered_scalar                                  &
!!     &         (flt_comm, nod_comm, node, filter, filter_smp,         &
!!     &          nnod_flt, i_filter, i_scalar, x_flt, nod_fld)
!!      subroutine cal_filtered_vector                                  &
!!     &         (flt_comm, nod_comm, node, filter, filter_smp,         &
!!     &          nnod_flt, num_filter_grp, id_filter_grp,              &
!!     &          i_filter, i_vect, x_flt, nod_fld)
!!      subroutine cal_filtered_sym_tensor                              &
!!     &         (flt_comm, nod_comm, node, filter, filter_smp,         &
!!     &          nnod_flt, num_filter_grp, id_filter_grp,              &
!!     &          i_filter, i_vect, x_flt, nod_fld)
!!        type(communication_table), intent(in) :: flt_comm, nod_comm
!!        type(node_data), intent(in) :: node
!!        type(filter_coefficients_type), intent(in) :: filter
!!        type(filter_coefficients_type), intent(in) :: filter_smp
!!        type(phys_data), intent(inout) :: nod_fld
!
      module select_filtering
!
      use m_precision
!
      use m_control_parameter
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_filter_coefficients
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_scalar                                    &
     &         (flt_comm, nod_comm, node, filter, filter_smp,           &
     &          nnod_flt, i_filter, i_scalar, x_flt, nod_fld)
!
      use cal_3d_filter_phys
      use cal_3d_filter_phys_smp
      use cal_line_filtering_scalar
      use copy_nodal_fields
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: flt_comm, nod_comm
      type(node_data), intent(in) :: node
      type(filter_coefficients_type), intent(in) :: filter, filter_smp
!
      integer(kind = kint), intent(in) :: nnod_flt, i_filter, i_scalar
!
      real(kind = kreal), intent(inout) :: x_flt(nnod_flt)
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING) then
!
        call cal_3d_ez_filter_scalar_phys                               &
     &     (flt_comm, nod_comm, node, filter, nnod_flt,                 &
     &      num_whole_filter_grp, id_whole_filter_grp,                  &
     &      i_scalar, i_filter, x_flt, nod_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
        call cal_3d_filter_scalar_phys_smp                              &
     &     (flt_comm, nod_comm, node, filter_smp, nnod_flt,             &
     &      num_whole_filter_grp, id_whole_filter_grp,                  &
     &      i_scalar, i_filter, x_flt, nod_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
        call cal_3d_ez_filter_scalar_smp                                &
     &     (flt_comm, nod_comm, node, filter_smp, nnod_flt,             &
     &      num_whole_filter_grp, id_whole_filter_grp,                  &
     &      i_scalar, i_filter, x_flt, nod_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
        call cal_3d_filter_scalar_phys                                  &
     &     (flt_comm, nod_comm, node, filter, nnod_flt,                 &
     &      num_whole_filter_grp, id_whole_filter_grp,                  &
     &      i_scalar, i_filter, x_flt, nod_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
!
        if (i_filter .ne. i_scalar) then
           call copy_scalar_component                                   &
     &        (node, nod_fld, i_scalar, i_filter)
        end if
        call cal_l_filtering_scalar(node%numnod, node%istack_nod_smp,   &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
        call scalar_send_recv(i_filter, nod_comm, nod_fld)
      end if
!
      end subroutine cal_filtered_scalar
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_vector                                    &
     &         (flt_comm, nod_comm, node, filter, filter_smp,           &
     &          nnod_flt, num_filter_grp, id_filter_grp,                &
     &          i_filter, i_vect, x_flt, nod_fld)
!
      use cal_3d_filter_phys
      use cal_3d_filter_phys_smp
      use cal_line_filtering_vector
      use copy_nodal_fields
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: flt_comm, nod_comm
      type(node_data), intent(in) :: node
      type(filter_coefficients_type), intent(in) :: filter, filter_smp
!
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
      integer (kind=kint), intent(in) :: nnod_flt, i_filter, i_vect
!
      real(kind = kreal), intent(inout) :: x_flt(3*nnod_flt)
      type(phys_data), intent(inout) :: nod_fld
!
!
      if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING ) then
        call cal_3d_ez_filter_vector_phys                               &
     &     (flt_comm, nod_comm, node, filter, nnod_flt,                 &
     &      num_filter_grp, id_filter_grp, i_vect, i_filter,            &
     &      x_flt, nod_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
        call cal_3d_filter_vector_phys_smp                              &
     &     (flt_comm, nod_comm, node, filter_smp, nnod_flt,             &
     &      num_filter_grp, id_filter_grp, i_vect, i_filter,            &
     &      x_flt, nod_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
        call cal_3d_ez_filter_vector_smp                                &
     &     (flt_comm, nod_comm, node, filter_smp, nnod_flt,             &
     &      num_filter_grp, id_filter_grp, i_vect, i_filter,            &
     &      x_flt, nod_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
        call cal_3d_filter_vector_phys                                  &
     &     (flt_comm, nod_comm, node, filter, nnod_flt,                 &
     &      num_filter_grp, id_filter_grp, i_vect, i_filter,            &
     &      x_flt, nod_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
!
        if (i_filter .ne. i_vect) then
          call copy_vector_component(node, nod_fld, i_vect, i_filter)
        end if
        call cal_l_filtering_vector(node%numnod, node%istack_nod_smp,   &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
        call vector_send_recv(i_filter, nod_comm, nod_fld)
      end if
!
      end subroutine cal_filtered_vector
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_sym_tensor                                &
     &         (flt_comm, nod_comm, node, filter, filter_smp,           &
     &          nnod_flt, num_filter_grp, id_filter_grp,                &
     &          i_filter, i_vect, x_flt, nod_fld)
!
      use cal_3d_filter_phys
      use cal_3d_filter_phys_smp
      use cal_line_filtering_tensor
      use copy_nodal_fields
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: flt_comm, nod_comm
      type(node_data), intent(in) :: node
      type(filter_coefficients_type), intent(in) :: filter, filter_smp
!
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
      integer (kind = kint), intent(in) :: nnod_flt, i_filter, i_vect
!
      real(kind = kreal), intent(inout) :: x_flt(6*nnod_flt)
      type(phys_data), intent(inout) :: nod_fld
!
!
      if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING ) then
!
        call cal_3d_ez_filter_tensor_phys                               &
     &     (flt_comm, nod_comm, node, filter,                           &
     &      nnod_flt, num_filter_grp, id_filter_grp,                    &
     &      i_vect, i_filter, x_flt, nod_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_3d_filter_tensor_phys_smp                              &
     &     (flt_comm, nod_comm, node, filter_smp, nnod_flt,             &
     &      num_filter_grp, id_filter_grp, i_vect, i_filter,            &
     &      x_flt, nod_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_3d_ez_filter_tensor_smp                                &
     &     (flt_comm, nod_comm, node, filter_smp, nnod_flt,             &
     &      num_filter_grp, id_filter_grp, i_vect, i_filter,            &
     &      x_flt, nod_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_3d_filter_tensor_phys                                  &
     &     (flt_comm, nod_comm, node, filter, nnod_flt,                 &
     &      num_filter_grp, id_filter_grp, i_vect, i_filter,            &
     &      x_flt, nod_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
        if (i_filter .ne. i_vect) then
          call copy_tensor_component(node, nod_fld, i_vect, i_filter)
        end if
        call cal_l_filtering_tensor(node%numnod, node%istack_nod_smp,   &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
        call sym_tensor_send_recv(i_filter, nod_comm, nod_fld)
      end if
!
      end subroutine cal_filtered_sym_tensor
!
! ----------------------------------------------------------------------
!
      end module select_filtering
