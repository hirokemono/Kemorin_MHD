!>@file  t_lic_repart_reference.f90
!!       module t_lic_repart_reference
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine set_lic_repart_reference_param                       &
!!     &         (new_part_ctl, each_part_p, rep_ref)
!!        type(new_patition_control), intent(in) :: new_part_ctl
!!        type(volume_partioning_param), intent(inout) :: each_part_p
!!        type(lic_repart_reference), intent(inout) :: rep_ref
!!
!!      subroutine init_lic_repart_ref                                  &
!!     &         (elps_LIC, mesh, pvr_rgb, each_part_p, rep_ref)
!!      subroutine alloc_lic_repart_ref(node, rep_ref)
!!      subroutine dealloc_lic_repart_ref(rep_ref)
!!      subroutine reset_lic_count_line_int(rep_ref)
!!        type(elapsed_lables), intent(in) :: elps_LIC
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(pvr_image_type), intent(in) :: pvr_rgb
!!        type(volume_partioning_param), intent(in) :: each_part_p
!!        type(lic_repart_reference), intent(inout) :: rep_ref
!!        type(mesh_geometry), intent(in) :: mesh
!!      subroutine output_LIC_line_integrate_count(time, elps_LIC,      &
!!     &                                           rep_ref)
!!        real(kind = kreal), intent(in) :: time
!!        type(elapsed_lables), intent(in) :: elps_LIC
!!        type(lic_repart_reference), intent(in) :: rep_ref
!!@endverbatim
!
      module t_lic_repart_reference
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_file_IO_parameter
      use calypso_mpi
!
      implicit  none
!
      character(len = kchara), parameter                                &
     &                        :: c_SAVED_COUNT =     'SAVED_COUNT'
      character(len = kchara), parameter                                &
     &                        :: c_PREDICTED_COUNT = 'PREDICTED_COUNT'
      character(len = kchara), parameter                                &
     &                        :: c_STACKED_COUNT =   'STACKED_COUNT'
      character(len = kchara), parameter                                &
     &                        :: c_AVERAGE_COUNT =   'AVERAGE_COUNT'
!
      character(len = kchara), parameter                                &
     &                        :: c_SAVED_TIME =     'SAVED_TIME'
      character(len = kchara), parameter                                &
     &                        :: c_PREDICTED_TIME = 'PREDICTED_TIME'
      character(len = kchara), parameter                                &
     &                        :: c_STACKED_TIME =   'STACKED_TIME'
      character(len = kchara), parameter                                &
     &                        :: c_AVERAGE_TIME =   'AVERAGE_TIME'
!
      integer(kind = kint), parameter :: i_SAVED_COUNT =     1
      integer(kind = kint), parameter :: i_PREDICTED_COUNT = 2
      integer(kind = kint), parameter :: i_STACKED_COUNT =   3
      integer(kind = kint), parameter :: i_AVERAGE_COUNT =   4
!
      character(len = kchara), parameter                                &
&              ::  def_lic_repart_ref_prefix = 'line_integrate_count'
!
!>  Structure for reference for LIC repartition
      type lic_repart_reference
!>    Number of node
        integer(kind = kint) :: iflag_repart_ref_type = 0
!
!>    Number of node
        integer(kind = kint) :: num_counts = 0
!>    Number of node
        integer(kind = kint) :: nnod_lic
!>    Work area for elapsed transfer time
        real(kind = kreal), allocatable :: count_line_int(:)
!>    Weight to to mix elapsed from previous
        real(kind = kreal) :: weight_prev = 1.0d0
!
!>    Average elapsed time for line integration
        real(kind = kreal) :: elapse_ray_trace(2)
!
        type(field_IO_params) :: file_IO
      end type lic_repart_reference
!
      character(len = kchara), parameter, private                       &
     &                :: line_int_cnt_name = 'line_intergration_count'
!
      private :: set_lic_repart_reference_file
      private :: copy_lic_repart_ref_from_IO, copy_lic_repart_ref_to_IO
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_lic_repart_reference_param                         &
     &         (new_part_ctl, each_part_p, rep_ref)
!
      use t_ctl_data_volume_grouping
      use t_control_param_vol_grping
      use m_file_format_switch
      use skip_comment_f
      use mpi_abort_by_missing_zlib
!
      type(new_patition_control), intent(in) :: new_part_ctl
      type(volume_partioning_param), intent(inout) :: each_part_p
      type(lic_repart_reference), intent(inout) :: rep_ref
!
      character(len = kchara) :: tmpchara
!
!
      if(each_part_p%iflag_repart_ref .ne. i_NO_REPARTITION) return
      if(new_part_ctl%partition_reference_ctl%iflag .eq. 0) return
!
      tmpchara = new_part_ctl%partition_reference_ctl%charavalue
      if(cmp_no_case(tmpchara,c_PREDICTED_COUNT)) then
        each_part_p%iflag_repart_ref = i_INT_COUNT_BASED
        rep_ref%iflag_repart_ref_type = i_PREDICTED_COUNT
      else if(cmp_no_case(tmpchara,c_STACKED_COUNT)) then
        each_part_p%iflag_repart_ref = i_INT_COUNT_BASED
        rep_ref%iflag_repart_ref_type = i_STACKED_COUNT
      else if(cmp_no_case(tmpchara,c_AVERAGE_COUNT)) then
        each_part_p%iflag_repart_ref = i_INT_COUNT_BASED
        rep_ref%iflag_repart_ref_type = i_AVERAGE_COUNT
!
      else if(cmp_no_case(tmpchara,c_PREDICTED_TIME)) then
        each_part_p%iflag_repart_ref = i_TIME_BASED
        rep_ref%iflag_repart_ref_type = i_PREDICTED_COUNT
      else if(cmp_no_case(tmpchara,c_STACKED_TIME)) then
        each_part_p%iflag_repart_ref = i_TIME_BASED
        rep_ref%iflag_repart_ref_type = i_STACKED_COUNT
      else if(cmp_no_case(tmpchara,c_AVERAGE_TIME)) then
        each_part_p%iflag_repart_ref = i_TIME_BASED
        rep_ref%iflag_repart_ref_type = i_AVERAGE_COUNT
      end if
!
      if      (each_part_p%iflag_repart_ref .eq. i_INT_COUNT_BASED      &
     &    .or. each_part_p%iflag_repart_ref .eq. i_TIME_BASED) then
        if(new_part_ctl%weight_to_previous_ctl%iflag .gt. 0) then
          rep_ref%weight_prev                                           &
     &         = new_part_ctl%weight_to_previous_ctl%realvalue
        end if
!
        rep_ref%file_IO%file_prefix = def_lic_repart_ref_prefix
        if(new_part_ctl%trace_count_head_ctl%iflag .gt. 0) then
          rep_ref%file_IO%file_prefix                                   &
     &      = new_part_ctl%trace_count_head_ctl%charavalue
        end if
        rep_ref%file_IO%iflag_format                                    &
     &      = choose_para_file_format(new_part_ctl%trace_count_fmt_ctl)
        call s_mpi_abort_by_missing_zlib(rep_ref%file_IO%file_prefix,   &
     &                                   rep_ref%file_IO%iflag_format)
      end if
!
      end subroutine set_lic_repart_reference_param
!
! -----------------------------------------------------------------------
!
      subroutine init_lic_repart_ref                                    &
     &         (elps_LIC, mesh, pvr_rgb, each_part_p, rep_ref)
!
      use m_work_time
      use t_mesh_data
      use t_time_data
      use t_field_data_IO
      use t_pvr_image_array
      use t_control_param_vol_grping
!
      use field_IO_select
      use int_volume_of_single_domain
!
      type(elapsed_lables), intent(in) :: elps_LIC
      type(mesh_geometry), intent(in) :: mesh
      type(pvr_image_type), intent(in) :: pvr_rgb
      type(volume_partioning_param), intent(in) :: each_part_p
!
      type(lic_repart_reference), intent(inout) :: rep_ref
!
      type(time_data) :: t_IO
      type(field_IO) :: fld_IO
      integer(kind = kint) :: ierr
!
!
      if(elps_LIC%flag_elapsed)                                         &
     &            call start_elapsed_time(elps_LIC%ist_elapsed+9)
      call alloc_lic_repart_ref(mesh%node, rep_ref)
!
      call set_lic_repart_reference_file(pvr_rgb, rep_ref)
      if(check_step_FEM_field_file(my_rank, iminus, rep_ref%file_IO))   &
     &                                                            then
        call sel_read_alloc_step_FEM_file                               &
     &     (nprocs, my_rank, iminus, rep_ref%file_IO, t_IO, fld_IO)
        call copy_lic_repart_ref_from_IO(t_IO, fld_IO,                  &
     &      rep_ref%nnod_lic, rep_ref%count_line_int,                   &
     &      rep_ref%num_counts, ierr)
        call dealloc_phys_IO(fld_IO)
!
        if(ierr .gt. 0) then
          write(e_message,'(2a)') 'Line integration counter for LIC',   &
     &                            'is wrong.'
          call calypso_mpi_abort(ierr, e_message)
        end if
      else
        if(my_rank .eq. 0) write(*,*)                                   &
     &         'Initialize line integration count by volume'
        call cal_node_volue_w_power(each_part_p%vol_power,              &
     &      mesh%node, mesh%ele, rep_ref%count_line_int)
      end if
      if(elps_LIC%flag_elapsed)                                         &
     &            call end_elapsed_time(elps_LIC%ist_elapsed+9)
!
      end subroutine init_lic_repart_ref
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_lic_repart_ref(rep_ref)
!
      type(lic_repart_reference), intent(inout) :: rep_ref
!
!
      if(allocated(rep_ref%count_line_int)) then
        deallocate(rep_ref%count_line_int)
      end if
!
      end subroutine dealloc_lic_repart_ref
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_lic_repart_reference_file(pvr_rgb, rep_ref)
!
      use t_pvr_image_array
!
      type(pvr_image_type), intent(in) :: pvr_rgb
      type(lic_repart_reference), intent(inout) :: rep_ref
!

!
      if(rep_ref%iflag_repart_ref_type .eq. i_PREDICTED_COUNT) then
        write(rep_ref%file_IO%file_prefix,'(2a)')                       &
     &          trim(pvr_rgb%pvr_prefix), '_predict_count'
      else if(rep_ref%iflag_repart_ref_type .eq. i_AVERAGE_COUNT) then
        write(rep_ref%file_IO%file_prefix,'(2a)')                       &
     &          trim(pvr_rgb%pvr_prefix), '_average_count'
      else if(rep_ref%iflag_repart_ref_type .eq. i_STACKED_COUNT) then
        write(rep_ref%file_IO%file_prefix,'(2a)')                       &
     &          trim(pvr_rgb%pvr_prefix), '_stacked_count'
      end if
!
      end subroutine set_lic_repart_reference_file
!
! -----------------------------------------------------------------------
!
      subroutine alloc_lic_repart_ref(node, rep_ref)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(lic_repart_reference), intent(inout) :: rep_ref
!
!
      rep_ref%num_counts = 0
      rep_ref%nnod_lic = node%numnod
      allocate(rep_ref%count_line_int(rep_ref%nnod_lic))
      if(rep_ref%nnod_lic .gt. 0) then
        call reset_lic_count_line_int(rep_ref)
      end if
!
      end subroutine alloc_lic_repart_ref
!
! -----------------------------------------------------------------------
!
      subroutine reset_lic_count_line_int(rep_ref)
!
      type(lic_repart_reference), intent(inout) :: rep_ref
!
!
!$omp parallel workshare
      rep_ref%count_line_int(1:rep_ref%nnod_lic) = 0.0d0
!$omp end parallel workshare
!
      end subroutine reset_lic_count_line_int
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_LIC_line_integrate_count(time, elps_LIC,        &
     &                                           rep_ref)
!
      use m_work_time
      use t_time_data
      use t_field_data_IO
      use field_IO_select
!
      real(kind = kreal), intent(in) :: time
      type(elapsed_lables), intent(in) :: elps_LIC
      type(lic_repart_reference), intent(in) :: rep_ref
!
      type(time_data) :: t_IO
      type(field_IO) :: fld_IO
!
!
      if(elps_LIC%flag_elapsed)                                         &
     &            call start_elapsed_time(elps_LIC%ist_elapsed+9)
      call copy_lic_repart_ref_to_IO(time, rep_ref, t_IO, fld_IO)
      call sel_write_step_FEM_field_file                                &
     &   (iminus, rep_ref%file_IO, t_IO, fld_IO)
      call dealloc_phys_IO(fld_IO)
      if(elps_LIC%flag_elapsed)                                         &
     &            call end_elapsed_time(elps_LIC%ist_elapsed+9)
!
      end subroutine output_LIC_line_integrate_count
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_lic_repart_ref_to_IO(time, rep_ref, t_IO, fld_IO)
!
      use t_time_data
      use t_field_data_IO
      use const_global_element_ids
!
      real(kind = kreal), intent(in) :: time
      type(lic_repart_reference), intent(in) :: rep_ref
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
!
      t_IO%i_time_step = rep_ref%num_counts
      t_IO%time =        time
      t_IO%dt =          0.0d0
!
      fld_IO%num_field_IO =      1
      call alloc_phys_name_IO(fld_IO)
!
      fld_IO%ntot_comp_IO =      1
      fld_IO%istack_comp_IO(0) = 0
      fld_IO%istack_comp_IO(1) = 1
      fld_IO%num_comp_IO(1) =    1
      fld_IO%fld_name(1) = line_int_cnt_name
!
      fld_IO%nnod_IO = rep_ref%nnod_lic
      call alloc_merged_field_stack(nprocs, fld_IO)
      call count_number_of_node_stack(fld_IO%nnod_IO,                   &
     &                                fld_IO%istack_numnod_IO)
!
      call alloc_phys_data_IO(fld_IO)
!
!$omp parallel workshare
      fld_IO%d_IO(1:fld_IO%nnod_IO,1)                                   &
     &          = rep_ref%count_line_int(1:fld_IO%nnod_IO)
!$omp end parallel workshare
!
      end subroutine copy_lic_repart_ref_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_lic_repart_ref_from_IO(t_IO, fld_IO,              &
     &          numnod, count_line_int, num_counts, ierr)
!
      use t_time_data
      use t_field_data_IO
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(in) :: fld_IO
      integer(kind = kint), intent(in) :: numnod
!
      real(kind = kreal), intent(inout) :: count_line_int(numnod)
      integer(kind = kint), intent(inout) :: num_counts
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      if(fld_IO%num_field_IO .ne. 1) ierr = 1
      if(fld_IO%ntot_comp_IO .ne. 1) ierr = 1
      if(fld_IO%fld_name(1) .ne. line_int_cnt_name) ierr = 1
      if(fld_IO%nnod_IO .ne. numnod) ierr = 1
!
!$omp parallel workshare
      count_line_int(1:numnod) = fld_IO%d_IO(1:numnod,1)
!$omp end parallel workshare
!
      num_counts = t_IO%i_time_step
!
      end subroutine copy_lic_repart_ref_from_IO
!
! -----------------------------------------------------------------------
!
      end module t_lic_repart_reference
