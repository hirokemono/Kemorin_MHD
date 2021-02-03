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
      use t_time_data
      use t_psf_results
      use t_ucd_data
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
!>      structure for section data
      type(psf_results), save :: psf_1
      type(psf_results), save :: psf_2
!
      type(time_data), save :: t_IO_u
      type(ucd_data), save:: psf_ucd
!
      integer(kind = kint) :: iflag
!
      integer(kind = kint) :: inod, iele, k1, ifld, icomp
      real(kind = kreal) :: rmsq
      real(kind = kreal), allocatable :: vmin(:), vmax(:), size(:)
      real(kind = kreal) :: scale, diff
!
      call read_ctl_file_psf_compares(0, psf_cmp_list1)
!
      num_psf_list = psf_cmp_list1%num_psf_cmp
      allocate(psf1_file_param(num_psf_list))
      allocate(psf2_file_param(num_psf_list))
      allocate(istep_psf(num_psf_list))
!
      do i = 1, num_psf_list
        call set_read_psf_file_ctl(default_psf_prefix,                  &
     &      psf_cmp_list1%psf_cmp_ctls(i)%first_psf%file_prefix_ctl,    &
     &      psf_cmp_list1%psf_cmp_ctls(i)%first_psf%file_format_ctl,    &
     &      psf1_file_param(i))
        call set_read_psf_file_ctl(default_psf_prefix,                  &
     &      psf_cmp_list1%psf_cmp_ctls(i)%second_psf%file_prefix_ctl,   &
     &      psf_cmp_list1%psf_cmp_ctls(i)%second_psf%file_format_ctl,   &
     &      psf2_file_param(i))
        istep_psf(i) = psf_cmp_list1%psf_cmp_ctls(i)%i_step_surface_ctl%intvalue
      end do
!
      do i = 1, num_psf_list
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
!
      do i = 1, num_psf_list
        call load_psf_data_to_link_IO                                   &
     &     (istep_psf(i), psf1_file_param(i), t_IO_u, psf_1, psf_ucd)
        call load_psf_data_to_link_IO                                   &
     &     (istep_psf(i), psf2_file_param(i), t_IO_u, psf_2, psf_ucd)
!
        iflag = 0
!        write(*,*) 'numnod', psf_1%psf_nod%numnod, psf_2%psf_nod%numnod
        if(psf_1%psf_nod%numnod .ne. psf_2%psf_nod%numnod) then
          write(*,*) 'Number of nodde in ',                             &
     &               trim(psf1_file_param(i)%file_prefix), ' and ',     &
     &               trim(psf2_file_param(i)%file_prefix), ' is wrong', &
     &               psf_1%psf_nod%numnod, psf_2%psf_nod%numnod
          iflag = 1
        end if
!
!        write(*,*) 'numele', psf_1%psf_ele%numele,                     &
!     &                       psf_2%psf_ele%numele
!        write(*,*) 'nnod_4_ele', psf_1%psf_ele%nnod_4_ele,             &
!     &                           psf_2%psf_ele%nnod_4_ele
        if(psf_1%psf_ele%numele .ne. psf_2%psf_ele%numele) then
          write(*,*) 'Number of element in ',                           &
     &               trim(psf1_file_param(i)%file_prefix), ' and ',     &
     &               trim(psf2_file_param(i)%file_prefix), ' is wrong', &
     &               psf_1%psf_ele%numele, psf_2%psf_ele%numele
          iflag = 1
        end if
        if(psf_1%psf_ele%nnod_4_ele .ne. psf_2%psf_ele%nnod_4_ele) then
          write(*,*) 'Element type in ',                                &
     &               trim(psf1_file_param(i)%file_prefix), ' and ',     &
     &               trim(psf2_file_param(i)%file_prefix), ' is wrong', &
     &               psf_1%psf_ele%nnod_4_ele, psf_2%psf_ele%nnod_4_ele
          iflag = 1
        end if
!
!        write(*,*) 'n_point', psf_1%psf_phys%n_point,                  &
!     &                        psf_2%psf_phys%n_point
!        write(*,*) 'num_phys', psf_1%psf_phys%num_phys,                &
!     &                        psf_2%psf_phys%num_phys
!        write(*,*) 'ntot_phys', psf_1%psf_phys%ntot_phys,              &
!     &                        psf_2%psf_phys%ntot_phys
        if(psf_1%psf_phys%n_point .ne. psf_2%psf_phys%n_point) then
          write(*,*) 'Number of point in field in ',                    &
     &               trim(psf1_file_param(i)%file_prefix), ' and ',     &
     &               trim(psf2_file_param(i)%file_prefix), ' is wrong', &
     &               psf_1%psf_phys%n_point, psf_2%psf_phys%n_point
          iflag = 1
        end if
        if(psf_1%psf_phys%num_phys .ne. psf_2%psf_phys%num_phys) then
          write(*,*) 'Number of field in ',                             &
     &               trim(psf1_file_param(i)%file_prefix), ' and ',     &
     &               trim(psf2_file_param(i)%file_prefix), ' is wrong', &
     &               psf_1%psf_phys%num_phys, psf_2%psf_phys%num_phys
          iflag = 1
        end if
        if(psf_1%psf_phys%ntot_phys .ne. psf_2%psf_phys%ntot_phys) then
          write(*,*) 'Number of total components in field in ',         &
     &               trim(psf1_file_param(i)%file_prefix), ' and ',     &
     &               trim(psf2_file_param(i)%file_prefix), ' is wrong', &
     &               psf_1%psf_phys%ntot_phys, psf_2%psf_phys%ntot_phys
          iflag = 1
        end if
!
        rmsq = 0.0d0
!$omp parallel do private(inod) reduction(+:rmsq)
        do inod = 1, psf_1%psf_nod%numnod
          rmsq =  rmsq                                                  &
     &          + psf_1%psf_nod%xx(inod,1) * psf_1%psf_nod%xx(inod,1)   &
     &          + psf_1%psf_nod%xx(inod,2) * psf_1%psf_nod%xx(inod,2)   &
     &          + psf_1%psf_nod%xx(inod,3) * psf_1%psf_nod%xx(inod,3)
        end do
!$omp end parallel do
!
        rmsq = sqrt(rmsq) / dble(psf_1%psf_nod%numnod)
!
        allocate(vmin(3))
        allocate(vmax(3))
        allocate(size(3))
!$omp parallel workshare
        vmin(1:3)= minval(psf_1%psf_nod%xx(1:psf_1%psf_nod%numnod,1:3),1)
        vmax(1:3)= maxval(psf_1%psf_nod%xx(1:psf_1%psf_nod%numnod,1:3),1)
!$omp end parallel workshare
        size(1:3) = vmax(1:3) - vmin(1:3)
        scale = sqrt(size(1)**2 + size(2)**2 + size(3)**2)
        scale = maxval(size)
!
        write(*,*) 'RMS of position', rmsq
        write(*,*) 'min of position', vmin
        write(*,*) 'max of position', vmax
        write(*,*) 'scale', scale
        deallocate(vmin, vmax, size)
!
        do icomp = 1, 3
          do inod = 1, psf_1%psf_nod%numnod
            diff = psf_2%psf_nod%xx(inod,icomp)                  &
     &               - psf_1%psf_nod%xx(inod,icomp)
            if((abs(diff) / scale) .gt. 1.0d-12) then
              write(*,*) icomp, 'componennt of ', inod, ' is differ', diff
              iflag = 1
            end if
          end do
        end do
!
        do iele = 1, psf_1%psf_ele%numele
          do k1 = 1, psf_1%psf_ele%nnod_4_ele
            if(psf_1%psf_ele%ie(iele,k1) .ne. psf_2%psf_ele%ie(iele,k1)) then
              write(*,*) 'connectivity at ', iele, k1, ' is differ',    &
     &            psf_1%psf_ele%ie(iele,k1), psf_2%psf_ele%ie(iele,k1)
              iflag = 1
            end if
          end do
        end do
!
        do ifld = 1, psf_1%psf_phys%num_phys
          if(psf_1%psf_phys%phys_name(ifld) .ne. psf_2%psf_phys%phys_name(ifld)) then
            write(*,*) 'field name at ', ifld, ' is differ: ',          &
     &            trim(psf_1%psf_phys%phys_name(ifld)), '  ',           &
     &            trim(psf_2%psf_phys%phys_name(ifld))
            iflag = 1
          end if
        end do
        do ifld = 1, psf_1%psf_phys%num_phys
          if(psf_1%psf_phys%num_component(ifld) .ne. psf_2%psf_phys%num_component(ifld)) then
            write(*,*) 'number of component at ', ifld, ' is differ: ', &
     &            psf_1%psf_phys%num_component(ifld), '  ',       &
     &            psf_2%psf_phys%num_component(ifld)
            iflag = 1
          end if
        end do
!
        ist = 0
        do ifld = 1, psf_1%psf_phys%num_phys
          ncomp = psf_1%psf_phys%num_component(ifld)
          allocate(vmin(ncomp))
          allocate(vmax(ncomp))
          allocate(size(ncomp))
!
!$omp parallel workshare
          vmin(1:ncomp) = minval(psf_1%psf_phys%d_fld(1:psf_1%psf_phys%n_point,ist+1:ist+ncomp),1)
          vmax(1:ncomp) = maxval(psf_1%psf_phys%d_fld(1:psf_1%psf_phys%n_point,ist+1:ist+ncomp),1)
!$omp end parallel workshare
          size(1:ncomp) = vmax(1:ncomp) - vmin(1:ncomp)
!
          scale = 0.0d0
          do icomp = 1, ncomp
            scale = scale + vmax(icomp)**2
          end do
          scale = sqrt(scale)
          write(*,*) ifld, 'scale for ', trim(psf_1%psf_phys%phys_name(ifld)), '  ', scale
!
          do inod = 1, psf_1%psf_phys%n_point
            do icomp = 1, ncomp
              diff = psf_2%psf_phys%d_fld(inod,ist+icomp)            &
     &                 - psf_1%psf_phys%d_fld(inod,ist+icomp)
              if((abs(diff) / scale) .gt. 1.0d-12) then
                write(*,*) ist+icomp, '-component at ', inod, ' is differ', diff
              iflag = 1
              end if
            end do
          end do
!
          deallocate(vmin, vmax, size)
          ist = ist + psf_1%psf_phys%num_component(ifld)
        end do
!
        if(iflag .eq. 0) then
          write(*,*) trim(psf1_file_param(i)%file_prefix), ' and ',     &
     &               trim(psf2_file_param(i)%file_prefix),              &
     &              ' have same data.'
        else
          write(*,*) trim(psf1_file_param(i)%file_prefix), ' and ',     &
     &               trim(psf2_file_param(i)%file_prefix),              &
     &              ' is different.'
        end if
!
        call dealloc_psf_results(psf_1)
        call dealloc_psf_results(psf_2)
      end do
!
!
      deallocate(psf1_file_param)
      deallocate(psf2_file_param)
      deallocate(istep_psf)
!
      end program compare_psf_files
