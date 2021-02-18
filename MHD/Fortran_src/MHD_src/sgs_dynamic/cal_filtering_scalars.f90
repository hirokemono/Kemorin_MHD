!
!      module cal_filtering_scalars
!
!      Written by H. Matsui
!
!!      subroutine cal_filtered_scalar_whole                            &
!!     &         (filter_param, nod_comm, node, filtering,              &
!!     &          i_filter, i_scalar, wk_filter, nod_fld, v_sol)
!!      subroutine cal_filtered_vector_whole                            &
!!     &         (filter_param, nod_comm, node, filtering,              &
!!     &          i_filter, i_vect, wk_filter, nod_fld, v_sol)
!!      subroutine cal_filtered_sym_tensor_whole                        &
!!     &         (filter_param, nod_comm, node, filtering,              &
!!     &          i_filter, i_vect, wk_filter, nod_fld, v_sol)
!!        type(SGS_filtering_params), intent(in) :: filter_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!          i_filter: field UD foe filtered field
!!          i_vect: original field ID
!
      module cal_filtering_scalars
!
      use m_precision
!
      use t_SGS_control_parameter
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_filtering_data
      use t_vector_for_solver
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
     &          i_filter, i_scalar, wk_filter, nod_fld, v_sol)
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
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call cal_filtered_scalar                                          &
     &   (filter_param%iflag_SGS_filter, filter_param%whole,            &
     &    nod_comm, node, filtering, i_filter, i_scalar,                &
     &    wk_filter, nod_fld, v_sol)
!
      end subroutine cal_filtered_scalar_whole
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_vector_whole                              &
     &         (filter_param, nod_comm, node, filtering,                &
     &          i_filter, i_vect, wk_filter, nod_fld, v_sol)
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
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call cal_filtered_vector                                          &
     &   (filter_param%iflag_SGS_filter, filter_param%whole,            &
     &    nod_comm, node, filtering, i_filter, i_vect,                  &
     &    wk_filter, nod_fld, v_sol)
!
      end subroutine cal_filtered_vector_whole
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_sym_tensor_whole                          &
     &         (filter_param, nod_comm, node, filtering,                &
     &          i_filter, i_vect, wk_filter, nod_fld, v_sol)
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
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call cal_filtered_sym_tensor                                      &
     &   (filter_param%iflag_SGS_filter, filter_param%whole,            &
     &    nod_comm, node, filtering, i_filter, i_vect,                  &
     &    wk_filter, nod_fld, v_sol)
!
      end subroutine cal_filtered_sym_tensor_whole
!
! ----------------------------------------------------------------------
!
      end module cal_filtering_scalars
