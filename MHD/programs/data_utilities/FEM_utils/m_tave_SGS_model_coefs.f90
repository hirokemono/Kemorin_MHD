!m_tave_SGS_model_coefs.f90
!      module m_tave_SGS_model_coefs
!
      module m_tave_SGS_model_coefs
!
!      Written by H. Matsui on May, 2008
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: num_comp, num_layer
!
      real(kind = kreal), allocatable :: coef(:,:)
      real(kind = kreal), allocatable :: ave_coef(:,:)
      real(kind = kreal), allocatable :: sigma_coef(:,:)
      character(len=kchara), allocatable :: comp_name(:)
!
      integer(kind = kint), parameter :: id_org_file = 37
      integer(kind = kint), parameter, private :: id_ave_file = 38
      integer(kind = kint), parameter, private :: id_sig_file = 39
!
!      subroutine allocate_model_coef_name
!      subroutine allocate_model_coef_array
!      subroutine deallocate_model_coef_array
!
!      subroutine write_t_ave_m_coef_file
!      subroutine read_t_ave_m_coef_file
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_model_coef_name
!
!
      allocate(comp_name(num_comp))
!
      end subroutine allocate_model_coef_name
!
!   --------------------------------------------------------------------
!
      subroutine allocate_model_coef_array
!
!
      allocate(coef(num_layer,num_comp))
      allocate(ave_coef(num_layer,num_comp))
      allocate(sigma_coef(num_layer,num_comp))
      coef = 0.0d0
      ave_coef  = 0.0d0
      sigma_coef  = 0.0d0
!
      end subroutine allocate_model_coef_array
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_model_coef_array
!
!
      deallocate(sigma_coef, ave_coef, coef, comp_name)
!
      end subroutine deallocate_model_coef_array
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_t_ave_m_coef_file(istep, time)
!
      use m_ctl_params_ele_grp_udt
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      character(len = kchara) :: file_name, fname_tmp
      integer(kind = kint) :: i
!
!
      call add_int_suffix                                               &
     &   (istep, tave_grp_ucd_param%file_prefix, fname_tmp)
      call add_dat_extension(fname_tmp, file_name)
      open (id_ave_file,file=file_name)
!
      write(id_ave_file,'(a)')   '# start and end step'
      write(id_ave_file,'(2i16)')  istep_start, istep
      write(id_ave_file,'(1p2e23.12)')  start_time,  time
      write(id_ave_file,'(a)')   '# number of component and layer'
      write(id_ave_file,'(2i16)')   num_comp, num_layer
!
      write(id_ave_file,'(a)', advance='no')                            &
     &        'end_step, end_time, layer_id, '
      do i = 1, num_comp
        write(id_ave_file,'(a,a2)', advance='no')                       &
     &                              trim(comp_name(i)), ', '
      end do
      write(id_ave_file,*)
!
      do i = 1, num_layer
        write(id_ave_file,'(i16,1pE25.15e3,i16,1p100E25.15e3)')         &
     &        istep, time, i,  ave_coef(i,1:num_comp)
      end do
!
      close(id_ave_file)
!
      end subroutine write_t_ave_m_coef_file
!
!   --------------------------------------------------------------------
!
      subroutine write_sigma_m_coef_file(istep, time)
!
      use m_ctl_params_ele_grp_udt
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
!
      character(len = kchara) :: file_name, fname_tmp
      integer(kind = kint) :: i
!
      call add_int_suffix                                               &
     &   (istep, sdev_grp_ucd_param%file_prefix, fname_tmp)
      call add_dat_extension(fname_tmp, file_name)
      open (id_sig_file,file=file_name)
!
      write(id_sig_file,'(a)')   '# start and end step'
      write(id_sig_file,'(2i16)')  istep_start, istep
      write(id_sig_file,'(1p2e23.15)')  start_time,  time
      write(id_ave_file,'(a)')   '# number of component and layer'
      write(id_sig_file,'(2i16)')  num_comp, num_layer
!
      write(id_sig_file,'(a)', advance='no')                            &
     &        'end_step, end_time, layer_id, '
      do i = 1, num_comp
        write(id_sig_file,'(a,a2)', advance='no')                       &
     &                              trim(comp_name(i)), ', '
      end do
      write(id_sig_file,*)
!
      do i = 1, num_layer
        write(id_sig_file,'(i16,1pE25.15e3,i16,1p100E25.15e3)')         &
     &        istep, time, i,  sigma_coef(i,1:num_comp)
      end do
!
      close(id_sig_file)
!
      end subroutine write_sigma_m_coef_file
!
!   --------------------------------------------------------------------
!
      end module m_tave_SGS_model_coefs
