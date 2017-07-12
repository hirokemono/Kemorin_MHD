!
!     module sgs_ini_model_coefs_IO
!
!     programmed by H.Matsui in 2005
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine output_ini_model_coefs                               &
!!     &         (i_step_sgs_coefs, i_step, time,                       &
!!     &          cmt_param, wk_sgs, wk_diff)
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(dynamic_model_data), intent(in) :: wk_sgs
!!      subroutine input_ini_model_coefs                                &
!!     &         (cmt_param, ele, fluid, layer_tbl, i_step_sgs_coefs,   &
!!     &          wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      module sgs_ini_model_coefs_IO
!
      use m_precision
!
      use calypso_mpi
      use m_constants
!
      use t_SGS_control_parameter
      use t_ele_info_4_dynamic
      use t_SGS_model_coefs
      use t_time_data
      use t_field_data_IO
!
      implicit none
!
      integer (kind = kint) :: iflag_rst_sgs_coef_code = 0
      integer (kind = kint) :: iflag_rst_sgs_comm_code = 0
      integer (kind = kint), parameter :: rst_sgs_coef_code = 18
      character(len=kchara), parameter                                  &
     &                      :: def_rst_sgs_coef =  'rst_model_coefs'
      character(len=kchara), parameter                                  &
     &                      :: def_rst_comm_coef = 'rst_diff_coefs'
!
      type(time_data), save :: Csim1_time
      type(field_IO), save :: Csim1_IO
      type(field_IO), save :: Cdiff1_IO
!
      integer (kind = kint) :: n_layer_d_IO
      integer (kind = kint) :: num_sgs_kinds_IO, num_diff_kinds_IO
      character(len=kchara), allocatable :: name_ak_sgs_IO(:)
      character(len=kchara), allocatable :: name_ak_diff_IO(:)
!
      real(kind=kreal), allocatable :: coef_sgs_IO(:,:)
      real(kind=kreal), allocatable :: coef_diff_IO(:,:)
!
      private :: n_layer_d_IO, num_sgs_kinds_IO, num_diff_kinds_IO
      private :: name_ak_sgs_IO, name_ak_diff_IO
      private :: coef_sgs_IO, coef_diff_IO
      private :: read_ini_model_coefs
      private :: set_ini_model_coefs_from_IO
      private :: set_initial_model_coefs_ele
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_sph_Csim_data                               &
     &         (init_d, rst_step, i_step_sgs_coefs, wk_sgs)
!
      use t_ele_info_4_dynamic
      use field_IO_select
!
      type(time_data), intent(in) :: init_d
      type(IO_step_param), intent(inout) :: rst_step
      type(dynamic_model_data), intent(inout) :: wk_sgs
      integer(kind = kint), intent(inout) :: i_step_sgs_coefs
!
      integer(kind = kint) :: ierr, istep_rst
!
!
      write(*,*) 'wk_sgs%nlayer', wk_sgs%nlayer
      write(*,*) 'wk_sgs%num_kinds', wk_sgs%num_kinds
      write(*,*) 'wk_sgs%ntot_comp', wk_sgs%ntot_comp
      write(*,*) 'wk_sgs%name', wk_sgs%name
      write(*,*) 'wk_sgs%fld_coef', size(wk_sgs%fld_coef,1), size(wk_sgs%fld_coef,2)
!
      if (init_d%i_time_step .eq. -1) then
        istep_rst = init_d%i_time_step
      else
        rst_step%istep_file = init_d%i_time_step / rst_step%increment
        istep_rst = rst_step%istep_file
      end if
!
      ierr = check_step_FEM_field_file(my_rank, istep_rst, Csim1_IO)
      if(ierr .gt. 0) then
        iflag_rst_sgs_coef_code = 0
        return
      end if
!
      call sel_read_alloc_step_FEM_file(nprocs, my_rank,                &
     &    rst_step%istep_file, Csim1_time, Csim1_IO)
!
      call set_sph_Csim_from_IO(Csim1_time, Csim1_IO, init_d,           &
     &    i_step_sgs_coefs, wk_sgs, ierr)
!
      call dealloc_phys_data_IO(Csim1_IO)
      call dealloc_phys_name_IO(Csim1_IO)
!
      end subroutine read_alloc_sph_Csim_data
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_Csim_data                                    &
     &        (i_step_sgs_coefs, rst_step, time_d, dynamic_SPH)
!
      use t_ele_info_4_dynamic
      use sph_filtering
      use field_IO_select
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: i_step_sgs_coefs
      type(IO_step_param), intent(in) :: rst_step
      type(time_data), intent(in) :: time_d
!
      type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
!
!
      call set_sph_Csim_to_IO                                           &
     &   (i_step_sgs_coefs, time_d, dynamic_SPH%wk_sgs,                 &
     &    Csim1_time, Csim1_IO)
!
      call sel_write_step_FEM_field_file(nprocs, my_rank,               &
     &    rst_step%istep_file, Csim1_time, Csim1_IO)
!
      call dealloc_phys_data_IO(Csim1_IO)
      call dealloc_phys_name_IO(Csim1_IO)
!
      end subroutine write_sph_Csim_data
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_Csim_from_IO(Csim_time, Csim_IO, time_d,       &
     &          i_step_sgs_coefs, wk_sgs, ierr)
!
      type(time_data), intent(in) :: Csim_time
      type(field_IO), intent(in) :: Csim_IO
      type(time_data), intent(in) :: time_d
!
      integer(kind = kint), intent(inout) :: ierr
      integer(kind = kint), intent(inout) :: i_step_sgs_coefs
      type(dynamic_model_data), intent(inout) :: wk_sgs
!
      integer(kind = kint) :: i_fld, j_fld
!
!
      ierr = 0
      if(time_d%i_time_step .ne. Csim_time%i_time_step) then
        e_message = 'Time step data in Csim restart file is wrong'
        ierr = 1
      end if
      if(time_d%time .ne. Csim_time%time) then
        e_message = 'Time data in Csim restart file is wrong'
        ierr = 1
      end if
      if(time_d%time .ne. Csim_time%time) then
        e_message = 'Time data in Csim restart file is wrong'
        ierr = 1
      end if
      if(wk_sgs%nlayer .ne. Csim_IO%nnod_IO) then
        e_message = 'number of node in Csim restart file is wrong'
        ierr = 1
      end if
      if(ierr .gt. 0) return
!
      if(i_step_sgs_coefs .eq. 0) then
        i_step_sgs_coefs = int(Csim_time%dt / time_d%dt)
      end if
!
      do i_fld = 1, wk_sgs%num_kinds
        do j_fld = 1, Csim_IO%num_field_IO
          if(Csim_IO%fld_name(j_fld) .eq. wk_sgs%name(i_fld)) then
!$omp parallel workshare
            wk_sgs%fld_coef(:,i_fld) = Csim_IO%d_IO(:,j_fld)
!$omp end parallel workshare
            exit
          end if
        end do
      end do
!
      end subroutine set_sph_Csim_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_Csim_to_IO(i_step_sgs_coefs, time_d, wk_sgs,   &
     &          Csim_time, Csim_IO)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: i_step_sgs_coefs
      type(time_data), intent(in) :: time_d
      type(dynamic_model_data), intent(in) :: wk_sgs
!
      type(time_data), intent(inout) :: Csim_time
      type(field_IO), intent(inout) :: Csim_IO
!
!
      Csim_time%i_time_step = time_d%i_time_step
      Csim_time%time = time_d%time
      Csim_time%dt = time_d%dt * dble(i_step_sgs_coefs)
!
      Csim_IO%nnod_IO =      wk_sgs%nlayer
      Csim_IO%num_field_IO = wk_sgs%num_kinds
      call alloc_phys_name_IO(Csim_IO)
!
      Csim_IO%fld_name(1:wk_sgs%num_kinds)                              &
     &      = wk_sgs%name(1:wk_sgs%num_kinds)
      Csim_IO%num_comp_IO(1:Csim_IO%num_field_IO) = 1
!
      call s_cal_total_and_stacks                                       &
     &   (Csim_IO%num_field_IO, Csim_IO%num_comp_IO, izero,             &
     &    Csim_IO%istack_comp_IO, Csim_IO%ntot_comp_IO)
!
      call alloc_phys_data_IO(Csim_IO)
!
!$omp parallel workshare
      Csim_IO%d_IO(:,:) = wk_sgs%fld_coef(:,:)
!$omp end parallel workshare
!
      end subroutine set_sph_Csim_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine output_ini_model_coefs                                 &
     &         (i_step_sgs_coefs, istep_rst, i_step, time,              &
     &          cmt_param, wk_sgs, wk_diff)
!
      use open_sgs_model_coefs
      use sgs_model_coefs_IO
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: istep_rst
      integer(kind=kint), intent(in) :: i_step
      real(kind=kreal), intent(in) :: time
!
      integer(kind = kint), intent(in) :: i_step_sgs_coefs
      type(commutation_control_params), intent(in) :: cmt_param
      type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!
      integer (kind = kint) :: inum
      character(len=kchara) :: file_name, fn_tmp
!
!
!
      if (my_rank .ne. 0) return
!
      if(istep_rst .lt. 0) then
        call add_elaps_postfix(Csim1_IO%file_prefix, fn_tmp)
      else
        call add_int_suffix(istep_rst, Csim1_IO%file_prefix, fn_tmp)
      end if
      call add_dat_extension(fn_tmp, file_name)
!
!
      open (rst_sgs_coef_code,file = file_name)
!
      write(rst_sgs_coef_code,'(a)')                                    &
     &              '! time step and interbal for dynamic model'
      write(rst_sgs_coef_code,'(2i16)') i_step, i_step_sgs_coefs
!
      write(rst_sgs_coef_code,'(a)')  '! num. of model coefs'
      write(rst_sgs_coef_code,'(2i16)')                                 &
     &       wk_sgs%num_kinds, wk_sgs%nlayer
!
      call write_sgs_coef_head(rst_sgs_coef_code, wk_sgs)
!
!   write model coefs for whole domain
      write(rst_sgs_coef_code,1000)  i_step, time, izero,               &
     &        wk_sgs%fld_whole_clip(1:wk_sgs%num_kinds)
!
!   write model coefs for each layer
      do inum = 1, wk_sgs%nlayer
        write(rst_sgs_coef_code,1000) i_step, time, inum,               &
     &       wk_sgs%fld_clip(inum,1:wk_sgs%num_kinds)
      end do
!
      if (cmt_param%iflag_commute .gt. id_SGS_commute_OFF) then
!
        write(rst_sgs_coef_code,'(a)')  '! num. of commute coefs'
        write(rst_sgs_coef_code,'(2i16)')                               &
     &       wk_diff%num_kinds, wk_diff%nlayer
!
        call write_diff_coef_head(rst_sgs_coef_code, wk_diff)
!
        write(rst_sgs_coef_code,1000) i_step, time, izero,              &
     &          wk_diff%fld_whole_clip(1:wk_diff%num_kinds)
!
        if (cmt_param%iset_DIFF_coefs .eq. 1 ) then
          do inum = 1, wk_diff%nlayer
            write(rst_sgs_coef_code,1000)  i_step, time, inum,          &
     &              wk_diff%fld_clip(inum,1:wk_diff%num_kinds)
          end do
        end if
      end if
      close (rst_sgs_coef_code)
!
 1000 format(i16,1pE25.15e3,i16,1p200E25.15e3)
!
      end subroutine output_ini_model_coefs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine input_ini_model_coefs(istep_rst, cmt_param,            &
     &          ele, fluid, layer_tbl, i_step_sgs_coefs,                &
     &          wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      use t_geometry_data
      use t_geometry_data_MHD
      use t_layering_ele_list
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: istep_rst
      type(commutation_control_params), intent(in) :: cmt_param
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
!
      integer(kind = kint), intent(inout) :: i_step_sgs_coefs
      type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      character(len=kchara) :: file_name, fn_tmp
!
!
      if(iflag_rst_sgs_coef_code .eq. 0) return
!
      if (istep_rst .lt. 0) then
        call add_elaps_postfix(Csim1_IO%file_prefix, fn_tmp)
      else
        call add_int_suffix(istep_rst, Csim1_IO%file_prefix, fn_tmp)
      end if
      call add_dat_extension(fn_tmp, file_name)
!
      open (rst_sgs_coef_code,file = file_name, status='old', ERR=99)
      call read_ini_model_coefs(cmt_param, i_step_sgs_coefs)
      close(rst_sgs_coef_code)
!
   99 continue
      num_sgs_kinds_IO = 0
      allocate( name_ak_sgs_IO(num_sgs_kinds_IO) )
      allocate( coef_sgs_IO(0:n_layer_d_IO,num_sgs_kinds_IO) )
      if (cmt_param%iflag_commute .gt. id_SGS_commute_OFF) then
        num_diff_kinds_IO = 0
        allocate( name_ak_diff_IO(num_diff_kinds_IO) )
        allocate( coef_diff_IO(0:n_layer_d_IO,num_diff_kinds_IO) )
      end if
!
      call set_ini_model_coefs_from_IO(cmt_param, wk_sgs, wk_diff)
      call set_initial_model_coefs_ele                                  &
     &   (cmt_param, ele, fluid, layer_tbl%e_grp,                       &
     &    wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      end subroutine input_ini_model_coefs
!
!-----------------------------------------------------------------------
!
      subroutine read_ini_model_coefs(cmt_param, i_step_sgs_coefs)
!
      use skip_comment_f
      use sgs_model_coefs_IO
!
      type(commutation_control_params), intent(in) :: cmt_param
      integer(kind = kint), intent(inout) :: i_step_sgs_coefs
!
      character(len=255) :: character_4_read
      integer (kind = kint) :: itmp, i_step
!
!
      call skip_comment(character_4_read, rst_sgs_coef_code)
      read(character_4_read,*) itmp, i_step_sgs_coefs
!
      call skip_comment(character_4_read, rst_sgs_coef_code)
      read(character_4_read,*) num_sgs_kinds_IO, n_layer_d_IO
!
      allocate( name_ak_sgs_IO(num_sgs_kinds_IO) )
      allocate( coef_sgs_IO(0:n_layer_d_IO,num_sgs_kinds_IO) )
      coef_sgs_IO = 0.0d0
!
      read(rst_sgs_coef_code,*) name_ak_sgs_IO(1:num_sgs_kinds_IO)
!
      call read_sgs_layerd_data(rst_sgs_coef_code, i_step,              &
     &    (n_layer_d_IO+1), num_sgs_kinds_IO, coef_sgs_IO(0,1) )
!
      if (cmt_param%iflag_commute .gt. id_SGS_commute_OFF) then
!
        call skip_comment(character_4_read, rst_sgs_coef_code)
        read(character_4_read,*) num_diff_kinds_IO, itmp
!
        allocate( name_ak_diff_IO(num_diff_kinds_IO) )
        read(rst_sgs_coef_code,*) name_ak_diff_IO(1:num_diff_kinds_IO)
!
        if (cmt_param%iset_DIFF_coefs .eq. 0) then
!
          allocate( coef_diff_IO(0:n_layer_d_IO,num_diff_kinds_IO) )
          coef_diff_IO = 0.0d0
!
          call read_sgs_layerd_data(rst_sgs_coef_code, i_step,         &
     &        (n_layer_d_IO+1), num_diff_kinds_IO, coef_diff_IO(0,1) )
!
        else
!
          allocate( coef_diff_IO(0:0,num_diff_kinds_IO) )
          coef_diff_IO = 0.0d0
!
          call read_sgs_layerd_data(rst_sgs_coef_code, i_step,         &
     &        ione, num_diff_kinds_IO, coef_diff_IO(0,1) )
!
        end if
      end if
!
      end subroutine read_ini_model_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine set_ini_model_coefs_from_IO                            &
     &         (cmt_param, wk_sgs, wk_diff)
!
      type(commutation_control_params), intent(in) :: cmt_param
      type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
!
      integer(kind = kint) :: i, j
!
!
      do i = 1, wk_sgs%num_kinds
        do j = 1, num_sgs_kinds_IO
          if ( wk_sgs%name(i) .eq. name_ak_sgs_IO(j) ) then
            wk_sgs%fld_clip(1:wk_sgs%nlayer,i)                          &
     &                 = coef_sgs_IO(1:wk_sgs%nlayer,j)
            wk_sgs%fld_whole_clip(i) =  coef_sgs_IO(0,j)
            exit
          end if
        end do
      end do
!
      deallocate(name_ak_sgs_IO)
      deallocate(coef_sgs_IO)
!
      if (cmt_param%iflag_commute .gt. id_SGS_commute_OFF) then
        do i = 1, wk_diff%num_kinds
          do j = 1, num_diff_kinds_IO
            if ( wk_diff%name(i) .eq. name_ak_diff_IO(j) ) then
              wk_diff%fld_whole_clip(i) = coef_diff_IO(0,j)
              if (cmt_param%iset_DIFF_coefs .eq. 1) then
                wk_diff%fld_clip(1:wk_diff%nlayer,i)                    &
     &                   = coef_diff_IO(1:wk_diff%nlayer,j)
              end if
              exit
            end if
          end do
        end do
!
        deallocate(name_ak_diff_IO)
        deallocate(coef_diff_IO)
!
      end if
!
      end subroutine set_ini_model_coefs_from_IO
!
!  ---------------------------------------------------------------------
!
      subroutine set_initial_model_coefs_ele                            &
     &         (cmt_param, ele, fluid, layer_egrp,                      &
     &          wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      use t_geometry_data
      use t_group_data
      use t_geometry_data_MHD
      use set_sgs_diff_model_coefs
!
      type(commutation_control_params), intent(in) :: cmt_param
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(group_data), intent(in) :: layer_egrp
      type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      integer(kind = kint) :: i, ist
!
      do i = 1, sgs_coefs%num_field
         ist = sgs_coefs%istack_comps(i-1) + 1
         call clear_model_coefs_2_ele(ele, sgs_coefs%num_comps(i), ist, &
     &      sgs_coefs%ntot_comp, sgs_coefs%ak)
         call set_model_coefs_2_ele(ele, izero, sgs_coefs%num_comps(i), &
     &       i, ist, layer_egrp%num_grp, layer_egrp%num_item,           &
     &       layer_egrp%istack_grp_smp, layer_egrp%item_grp,            &
     &       sgs_coefs%num_field, sgs_coefs%ntot_comp,                  &
     &       wk_sgs%fld_clip, wk_sgs%comp_clip, sgs_coefs%ak)
      end do
!
      if (cmt_param%iflag_commute .gt. id_SGS_commute_OFF) then
        if (cmt_param%iset_DIFF_coefs .eq. 0) then
          do i = 1, diff_coefs%num_field
            call set_diff_coefs_whole_ele                               &
     &         (ele, fluid%istack_ele_fld_smp, i, diff_coefs%ntot_comp, &
     &          wk_diff%fld_whole_clip, diff_coefs%ak)
          end do
        else
          do i = 1, diff_coefs%num_field
            call set_diff_coefs_layer_ele                               &
     &         (ele, i, layer_egrp%num_grp, layer_egrp%num_item,        &
     &          layer_egrp%istack_grp_smp, layer_egrp%item_grp,         &
     &          diff_coefs%ntot_comp, wk_diff%fld_clip, diff_coefs%ak)
          end do
        end if
      end if
!
      end subroutine set_initial_model_coefs_ele
!
!-----------------------------------------------------------------------
!
      end module sgs_ini_model_coefs_IO
