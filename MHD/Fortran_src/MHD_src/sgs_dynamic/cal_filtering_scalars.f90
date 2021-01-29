!
!      module cal_filtering_scalars
!
!      Written by H. Matsui
!
!!      subroutine cal_filtered_scalar_whole                            &
!!     &         (filter_param, nod_comm, node, filtering,              &
!!     &          i_filter, i_scalar, wk_filter, nod_fld)
!!      subroutine cal_filtered_vector_whole                            &
!!     &         (filter_param, nod_comm, node, filtering,              &
!!     &          i_filter, i_vect, wk_filter, nod_fld)
!!      subroutine cal_filtered_sym_tensor_whole                        &
!!     &         (filter_param, nod_comm, node, filtering,              &
!!     &          i_filter, i_vect, wk_filter, nod_fld)
!!        type(SGS_filtering_params), intent(in) :: filter_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(phys_data), intent(inout) :: nod_fld
!!          i_filter: field UD foe filtered field
!!          i_vect: original field ID
!
      module cal_filtering_scalars
!
      use m_precision
!
      use m_nod_filter_comm_table
      use t_SGS_control_parameter
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_filtering_data
      use m_array_for_send_recv
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_scalar_whole                              &
     &         (filter_param, nod_comm, node, filtering,                &
     &          i_filter, i_scalar, wk_filter, nod_fld)
!
      use select_filtering
!
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
      integer (kind=kint), intent(in) :: i_filter, i_scalar
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_filtered_scalar                                          &
     &   (filter_param%iflag_SGS_filter, filter_param%whole,            &
     &    nod_comm, node, filtering, i_filter, i_scalar,                &
     &    wk_filter, nod_fld, vect1)
!
      end subroutine cal_filtered_scalar_whole
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_vector_whole                              &
     &         (filter_param, nod_comm, node, filtering,                &
     &          i_filter, i_vect, wk_filter, nod_fld)
!
      use select_filtering
!
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
      integer (kind=kint), intent(in) :: i_filter, i_vect
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_filtered_vector                                          &
     &   (filter_param%iflag_SGS_filter, filter_param%whole,            &
     &    nod_comm, node, filtering, i_filter, i_vect,                  &
     &    wk_filter, nod_fld, vect1)
!
      end subroutine cal_filtered_vector_whole
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_sym_tensor_whole                          &
     &         (filter_param, nod_comm, node, filtering,                &
     &          i_filter, i_vect, wk_filter, nod_fld)
!
      use select_filtering
!
      type(SGS_filtering_params), intent(in) :: filter_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
      integer (kind = kint), intent(in) :: i_filter, i_vect
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_filtered_sym_tensor                                      &
     &   (filter_param%iflag_SGS_filter, filter_param%whole,            &
     &    nod_comm, node, filtering, i_filter, i_vect,                  &
     &    wk_filter, nod_fld, vect1)
!
      end subroutine cal_filtered_sym_tensor_whole
!
! ----------------------------------------------------------------------
!
      end module cal_filtering_scalars
