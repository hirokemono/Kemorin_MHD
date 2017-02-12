!
!      module select_filtering
!
!      Written by H. Matsui
!
!!      subroutine cal_filtered_scalar                                  &
!!     &         (iflag_filter_mode, f_area, nod_comm, node, filtering, &
!!     &          i_filter, i_scalar, wk_filter, nod_fld)
!!      subroutine cal_filtered_vector                                  &
!!     &         (iflag_filter_mode, f_area, nod_comm, node, filtering, &
!!     &          i_filter, i_vect, wk_filter, nod_fld)
!!      subroutine cal_filtered_sym_tensor                              &
!!     &         (iflag_filter_mode, f_area, nod_comm, node, filtering, &
!!     &          i_filter, i_vect, wk_filter, nod_fld)
!!        type(SGS_filter_area_params), intent(in) :: f_area
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(phys_data), intent(inout) :: nod_fld
!
      module select_filtering
!
      use m_precision
!
      use t_SGS_control_parameter
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_filtering_data
      use t_filter_coefficients
      use t_l_filtering_data
!
      implicit none
!
      private :: line_filtered_scalar,  line_filtered_vector
      private :: line_filtered_sym_tensor
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_scalar                                    &
     &         (iflag_filter_mode, f_area, nod_comm, node, filtering,   &
     &          i_filter, i_scalar, wk_filter, nod_fld)
!
      use cal_3d_filter_phys
      use cal_3d_filter_phys_smp
!
      type(SGS_filter_area_params), intent(in) :: f_area
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
!
      integer(kind = kint), intent(in) :: iflag_filter_mode
      integer(kind = kint), intent(in) :: i_filter, i_scalar
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_filter_mode .eq. id_SGS_3D_EZ_FILTERING) then
        call cal_3d_ez_filter_scalar_phys(filtering%comm,               &
     &      nod_comm, node, filtering%filter, wk_filter%nnod_fil,       &
     &      f_area%num_f_group, f_area%id_f_group, i_scalar, i_filter,  &
     &      wk_filter%x_fil(1), nod_fld)
!
      else if ( iflag_filter_mode .eq. id_SGS_3D_SMP_FILTERING ) then
        call cal_3d_filter_scalar_phys_smp(filtering%comm,              &
     &      nod_comm, node, filtering%filter_smp, wk_filter%nnod_fil,   &
     &      f_area%num_f_group, f_area%id_f_group, i_scalar, i_filter,  &
     &      wk_filter%x_fil(1), nod_fld)
!
      else if ( iflag_filter_mode .eq. id_SGS_3D_EZ_SMP_FILTERING) then
        call cal_3d_ez_filter_scalar_smp(filtering%comm,                &
     &      nod_comm, node, filtering%filter_smp, wk_filter%nnod_fil,   &
     &      f_area%num_f_group, f_area%id_f_group, i_scalar, i_filter,  &
     &      wk_filter%x_fil(1), nod_fld)
!
      else if ( iflag_filter_mode .eq. id_SGS_3D_FILTERING) then
        call cal_3d_filter_scalar_phys(filtering%comm,                  &
     &      nod_comm, node, filtering%filter, wk_filter%nnod_fil,       &
     &      f_area%num_f_group, f_area%id_f_group, i_scalar, i_filter,  &
     &      wk_filter%x_fil(1), nod_fld)
!
      else if ( iflag_filter_mode .eq. id_SGS_LINE_FILTERING) then
        call line_filtered_scalar(nod_comm, node, filtering%fil_l_smp,  &
     &      wk_filter%nnod_fil, i_filter, i_scalar,                     &
     &      wk_filter%x_fil(1), nod_fld)
      end if
!
      end subroutine cal_filtered_scalar
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_vector                                    &
     &         (iflag_filter_mode, f_area, nod_comm, node, filtering,   &
     &          i_filter, i_vect, wk_filter, nod_fld)
!
      use cal_3d_filter_phys
      use cal_3d_filter_phys_smp
!
      type(SGS_filter_area_params), intent(in) :: f_area
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
!
      integer(kind = kint), intent(in) :: iflag_filter_mode
      integer (kind=kint), intent(in) :: i_filter, i_vect
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
      if ( iflag_filter_mode .eq. id_SGS_3D_EZ_FILTERING ) then
        call cal_3d_ez_filter_vector_phys(filtering%comm,               &
     &      nod_comm, node, filtering%filter, wk_filter%nnod_fil,       &
     &      f_area%num_f_group, f_area%id_f_group, i_vect, i_filter,    &
     &      wk_filter%x_fil(1), nod_fld)
!
      else if ( iflag_filter_mode .eq. id_SGS_3D_SMP_FILTERING ) then
        call cal_3d_filter_vector_phys_smp(filtering%comm,              &
     &      nod_comm, node, filtering%filter_smp, wk_filter%nnod_fil,   &
     &      f_area%num_f_group, f_area%id_f_group, i_vect, i_filter,    &
     &      wk_filter%x_fil(1), nod_fld)
!
      else if ( iflag_filter_mode .eq. id_SGS_3D_EZ_SMP_FILTERING) then
        call cal_3d_ez_filter_vector_smp(filtering%comm,                &
     &      nod_comm, node, filtering%filter_smp, wk_filter%nnod_fil,   &
     &      f_area%num_f_group, f_area%id_f_group, i_vect, i_filter,    &
     &      wk_filter%x_fil(1), nod_fld)
!
      else if ( iflag_filter_mode .eq. id_SGS_3D_FILTERING) then
        call cal_3d_filter_vector_phys(filtering%comm,                  &
     &      nod_comm, node, filtering%filter, wk_filter%nnod_fil,       &
     &      f_area%num_f_group, f_area%id_f_group, i_vect, i_filter,    &
     &      wk_filter%x_fil(1), nod_fld)
!
      else if ( iflag_filter_mode .eq. id_SGS_LINE_FILTERING) then
        call line_filtered_vector(nod_comm, node, filtering%fil_l_smp,  &
     &      wk_filter%nnod_fil, i_filter, i_vect, wk_filter%x_fil(1),   &
     &      nod_fld)
      end if
!
      end subroutine cal_filtered_vector
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_sym_tensor                                &
     &         (iflag_filter_mode, f_area, nod_comm, node, filtering,   &
     &          i_filter, i_vect, wk_filter, nod_fld)
!
      use cal_3d_filter_phys
      use cal_3d_filter_phys_smp
!
      type(SGS_filter_area_params), intent(in) :: f_area
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(filtering_data_type), intent(in) :: filtering
!
      integer(kind = kint), intent(in) :: iflag_filter_mode
      integer(kind = kint), intent(in) :: i_filter, i_vect
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(phys_data), intent(inout) :: nod_fld
!
!
      if ( iflag_filter_mode .eq. id_SGS_3D_EZ_FILTERING ) then
        call cal_3d_ez_filter_tensor_phys(filtering%comm,               &
     &      nod_comm, node, filtering%filter, wk_filter%nnod_fil,       &
     &      f_area%num_f_group, f_area%id_f_group, i_vect, i_filter,    &
     &      wk_filter%x_fil(1), nod_fld)
!
      else if ( iflag_filter_mode .eq. id_SGS_3D_SMP_FILTERING ) then
        call cal_3d_filter_tensor_phys_smp(filtering%comm,              &
     &      nod_comm, node, filtering%filter_smp, wk_filter%nnod_fil,   &
     &     f_area%num_f_group, f_area%id_f_group, i_vect, i_filter,     &
     &      wk_filter%x_fil(1), nod_fld)
!
      else if ( iflag_filter_mode .eq. id_SGS_3D_EZ_SMP_FILTERING) then
        call cal_3d_ez_filter_tensor_smp(filtering%comm,                &
     &     nod_comm, node, filtering%filter_smp, wk_filter%nnod_fil,    &
     &     f_area%num_f_group, f_area%id_f_group, i_vect, i_filter,     &
     &      wk_filter%x_fil(1), nod_fld)
!
      else if ( iflag_filter_mode .eq. id_SGS_3D_FILTERING) then
        call cal_3d_filter_tensor_phys(filtering%comm,                  &
     &      nod_comm, node, filtering%filter, wk_filter%nnod_fil,       &
     &      f_area%num_f_group, f_area%id_f_group, i_vect, i_filter,    &
     &      wk_filter%x_fil(1), nod_fld)
!
      else if ( iflag_filter_mode .eq. id_SGS_LINE_FILTERING) then
        call line_filtered_sym_tensor                                   &
     &     (nod_comm, node, filtering%fil_l_smp,                        &
     &      wk_filter%nnod_fil, i_filter, i_vect, wk_filter%x_fil(1),   &
     &      nod_fld)
      end if
!
      end subroutine cal_filtered_sym_tensor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine line_filtered_scalar(nod_comm, node, fil_l_smp,        &
     &          nnod_flt, i_filter, i_scalar, x_flt, nod_fld)
!
      use cal_line_filtering_vector
      use copy_nodal_fields
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(line_filtering_type), intent(in) :: fil_l_smp
!
      integer(kind = kint), intent(in) :: nnod_flt, i_filter, i_scalar
!
      real(kind = kreal), intent(inout) :: x_flt(nnod_flt)
      type(phys_data), intent(inout) :: nod_fld
!
!
        if (i_filter .ne. i_scalar) then
           call copy_scalar_component(nod_fld, i_scalar, i_filter)
        end if
        call cal_l_filtering_scalar(node%numnod, node%istack_nod_smp,   &
     &      fil_l_smp%nmax_lf, fil_l_smp%ntot_lf, fil_l_smp%nsize_smp,  &
     &      fil_l_smp%inod_lf, fil_l_smp%istack_lf, fil_l_smp%item_lf,  &
     &      fil_l_smp%coef_l, nod_fld%ntot_phys, i_filter,              &
     &      nod_fld%d_fld, x_flt(1))
        call scalar_send_recv(i_filter, nod_comm, nod_fld)
!
      end subroutine line_filtered_scalar
!
! ----------------------------------------------------------------------
!
      subroutine line_filtered_vector(nod_comm, node, fil_l_smp,        &
     &          nnod_flt, i_filter, i_vect, x_flt, nod_fld)
!
      use cal_line_filtering_vector
      use copy_nodal_fields
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(line_filtering_type), intent(in) :: fil_l_smp
!
      integer (kind=kint), intent(in) :: nnod_flt, i_filter, i_vect
!
      real(kind = kreal), intent(inout) :: x_flt(3*nnod_flt)
      type(phys_data), intent(inout) :: nod_fld
!
!
        if (i_filter .ne. i_vect) then
          call copy_vector_component(nod_fld, i_vect, i_filter)
        end if
        call cal_l_filtering_vector(node%numnod, node%istack_nod_smp,   &
     &      fil_l_smp%nmax_lf, fil_l_smp%ntot_lf, fil_l_smp%nsize_smp,  &
     &      fil_l_smp%inod_lf, fil_l_smp%istack_lf, fil_l_smp%item_lf,  &
     &      fil_l_smp%coef_l, nod_fld%ntot_phys, i_filter,              &
     &      nod_fld%d_fld, x_flt(1))
        call vector_send_recv(i_filter, nod_comm, nod_fld)
!
      end subroutine line_filtered_vector
!
! ----------------------------------------------------------------------
!
      subroutine line_filtered_sym_tensor(nod_comm, node, fil_l_smp,    &
     &          nnod_flt, i_filter, i_vect, x_flt, nod_fld)
!
      use cal_line_filtering_vector
      use copy_nodal_fields
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(line_filtering_type), intent(in) :: fil_l_smp
!
      integer(kind = kint), intent(in) :: nnod_flt, i_filter, i_vect
!
      real(kind = kreal), intent(inout) :: x_flt(6*nnod_flt)
      type(phys_data), intent(inout) :: nod_fld
!
!
        if (i_filter .ne. i_vect) then
          call copy_tensor_component(nod_fld, i_vect, i_filter)
        end if
        call cal_l_filtering_tensor(node%numnod, node%istack_nod_smp,   &
     &      fil_l_smp%nmax_lf, fil_l_smp%ntot_lf, fil_l_smp%nsize_smp,  &
     &      fil_l_smp%inod_lf, fil_l_smp%istack_lf, fil_l_smp%item_lf,  &
     &      fil_l_smp%coef_l, nod_fld%ntot_phys, i_filter,              &
     &      nod_fld%d_fld, x_flt(1))
        call sym_tensor_send_recv(i_filter, nod_comm, nod_fld)
!
      end subroutine line_filtered_sym_tensor
!
! ----------------------------------------------------------------------
!
      end module select_filtering
