!>@file   compare_psf_files.f90
!!@brief  module compare_psf_files
!!
!!@date  Programmed by H.Matsui in Jan., 2021
!
!>@brief control data for cross sections
!
      program compare_psf_files
!
      use m_precision
      use t_ctl_data_psf_compares
!
      use t_file_IO_parameter
      use t_control_params_4_psf
!
      type(psf_compare_controls), save :: psf_cmp_list1
!
      character(len=kchara), parameter :: default_psf_prefix = 'psf'
!
      integer(kind = kint) :: num_psf_list
      type(field_IO_params), allocatable, save:: psf1_file_param(:)
      type(field_IO_params), allocatable, save :: psf2_file_param(:)
      integer(kind = kint), allocatable :: istep_psf(:)
!
!
      call read_ctl_file_psf_compares(0, psf_cmp_list1)
!
      num_psf_list = psf_cmp_list1%num_psf_cmp
      allocate(psf1_file_param(num_psf_list))
      allocate(psf2_file_param(num_psf_list))
      allocate(istep_psf(num_psf_list))
!
      do i = 1, num_psf_list
        call set_merged_psf_file_ctl(default_psf_prefix,                &
     &      psf_cmp_list1%psf_cmp_ctls(i)%first_psf%file_prefix_ctl,    &
     &      psf_cmp_list1%psf_cmp_ctls(i)%first_psf%file_format_ctl,    &
     &      psf1_file_param(i))
        call set_merged_psf_file_ctl(default_psf_prefix,                &
     &      psf_cmp_list1%psf_cmp_ctls(i)%second_psf%file_prefix_ctl,   &
     &      psf_cmp_list1%psf_cmp_ctls(i)%second_psf%file_format_ctl,   &
     &      psf2_file_param(i))
        istep_psf(i) = psf_cmp_list1%psf_cmp_ctls(i)%i_step_surface_ctl%intvalue
!
        write(*,*) 'Compare list ', i
        write(*,*) 'psf1_file_param%file_prefix: ',                     &
     &          trim(psf1_file_param(i)%file_prefix)
        write(*,*) 'psf1_file_param%iflag_format: ',                    &
     &          psf1_file_param(i)%iflag_format,                        &
     &    trim(psf_cmp_list1%psf_cmp_ctls(i)%first_psf%file_format_ctl%charavalue)
        write(*,*) 'psf2_file_param%file_prefix: ',                     &
     &          trim(psf2_file_param(i)%file_prefix)
        write(*,*) 'psf2_file_param%iflag_format: ',                    &
     &          psf2_file_param(i)%iflag_format,                        &
     &    trim(psf_cmp_list1%psf_cmp_ctls(i)%second_psf%file_format_ctl%charavalue)
        write(*,*) 'istep_psf(i): ', istep_psf(i)
      end do
!
      call dealloc_psf_compares_ctl(psf_cmp_list1)
      deallocate(psf1_file_param)
      deallocate(psf2_file_param)
      deallocate(istep_psf)
!
      end program compare_psf_files
