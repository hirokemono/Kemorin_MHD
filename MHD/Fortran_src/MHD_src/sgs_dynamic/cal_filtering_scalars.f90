!
!      module cal_filtering_scalars
!
!      Written by H. Matsui
!
!!      subroutine cal_filtered_scalar_whole                            &
!!     &         (nod_comm, node, filtering, i_filter, i_scalar,        &
!!     &          wk_filter, nod_fld)
!!      subroutine cal_filtered_vector_whole                            &
!!     &         (nod_comm, node, filtering, i_filter, i_vect,          &
!!     &          wk_filter, nod_fld)
!!      subroutine cal_filtered_sym_tensor_whole                        &
!!     &         (nod_comm, node, filtering, i_filter, i_vect,          &
!!     &          wk_filter, nod_fld)
!!
!!      subroutine cal_filtered_scalar_in_fluid                         &
!!     &         (nod_comm, node, filtering, i_filter, i_scalar,        &
!!     &          wk_filter, nod_fld)
!!      subroutine cal_filtered_vector_in_fluid                         &
!!     &         (nod_comm, node, filtering, i_filter, i_vect,          &
!!     &          wk_filter, nod_fld)
!!      subroutine cal_filtered_tensor_in_fluid                         &
!!     &         (nod_comm, node, filtering, i_filter, i_vect,          &
!!     &          wk_filter, nod_fld)
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
      use m_control_parameter
      use m_nod_filter_comm_table
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_filtering_data
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
     &         (nod_comm, node, filtering, i_filter, i_scalar,          &
     &          wk_filter, nod_fld)
!
      use select_filtering
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
      integer (kind=kint), intent(in) :: i_filter, i_scalar
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_filtered_scalar(filtering%comm, nod_comm, node,          &
     &    filtering%filter, filtering%filter_smp, wk_filter%nnod_fil,   &
     &    num_whole_filter_grp, id_whole_filter_grp,                    &
     &    i_filter, i_scalar, wk_filter%x_fil, nod_fld)
!
      end subroutine cal_filtered_scalar_whole
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_vector_whole                              &
     &         (nod_comm, node, filtering, i_filter, i_vect,            &
     &          wk_filter, nod_fld)
!
      use select_filtering
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
      integer (kind=kint), intent(in) :: i_filter, i_vect
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_filtered_vector(filtering%comm, nod_comm, node,          &
     &    filtering%filter, filtering%filter_smp, wk_filter%nnod_fil,   &
     &    num_whole_filter_grp, id_whole_filter_grp,                    &
     &    i_filter, i_vect, wk_filter%x_fil, nod_fld)
!
      end subroutine cal_filtered_vector_whole
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_sym_tensor_whole                          &
     &         (nod_comm, node, filtering, i_filter, i_vect,            &
     &          wk_filter, nod_fld)
!
      use select_filtering
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
      integer (kind = kint), intent(in) :: i_filter, i_vect
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_filtered_sym_tensor(filtering%comm, nod_comm, node,      &
     &    filtering%filter, filtering%filter_smp, wk_filter%nnod_fil,   &
     &    num_whole_filter_grp, id_whole_filter_grp,                    &
     &    i_filter, i_vect, wk_filter%x_fil, nod_fld)
!
      end subroutine cal_filtered_sym_tensor_whole
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_scalar_in_fluid                           &
     &         (nod_comm, node, filtering, i_filter, i_scalar,          &
     &          wk_filter, nod_fld)
!
      use select_filtering
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
      integer (kind=kint), intent(in) :: i_filter, i_scalar
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_filtered_scalar(filtering%comm, nod_comm, node,          &
     &    filtering%filter, filtering%filter_smp, wk_filter%nnod_fil,   &
     &    num_fluid_filter_grp, id_fluid_filter_grp,                    &
     &    i_filter, i_scalar, wk_filter%x_fil, nod_fld)
!
      end subroutine cal_filtered_scalar_in_fluid
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_vector_in_fluid                           &
     &         (nod_comm, node, filtering, i_filter, i_vect,            &
     &          wk_filter, nod_fld)
!
!
       use select_filtering
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
      integer (kind=kint), intent(in) :: i_filter, i_vect
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_filtered_vector(filtering%comm, nod_comm, node,          &
     &    filtering%filter, filtering%filter_smp, wk_filter%nnod_fil,   &
     &    num_fluid_filter_grp, id_fluid_filter_grp,                    &
     &    i_filter, i_vect, wk_filter%x_fil, nod_fld)
!
      end subroutine cal_filtered_vector_in_fluid
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_tensor_in_fluid                           &
     &         (nod_comm, node, filtering, i_filter, i_vect,            &
     &          wk_filter, nod_fld)
!
      use select_filtering
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
      integer (kind=kint), intent(in) :: i_filter, i_vect
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_filtered_sym_tensor(filtering%comm, nod_comm, node,      &
     &    filtering%filter, filtering%filter_smp, wk_filter%nnod_fil,   &
     &    num_fluid_filter_grp, id_fluid_filter_grp,                    &
     &    i_filter, i_vect, wk_filter%x_fil, nod_fld)
!
      end subroutine cal_filtered_tensor_in_fluid
!
! ----------------------------------------------------------------------
!
      end module cal_filtering_scalars
