!
!     program psf_rms_aves
!
!      program for pick up surface connectivities form subdomain mesh
!         programmed  by  H.Matsui (U. Chicago)  on Oct. 2003 (ver 1.0)
!         Modified  by  H.Matsui (U. Chicago)  on Jan. 2007 (ver 2.0)
!
      program psf_rms_aves
!
      use m_precision
      use m_constants
!
      use m_psf_results
      use m_field_file_format
!
      use t_file_IO_parameter
      use t_time_data
      use t_ucd_data
!
      use set_parallel_file_name
      use ucd_IO_select
      use cal_psf_rms_aves
      use take_avarages_4_psf
      use take_normals_4_psf
!
      implicit    none
!
      type(field_IO_params), save :: ave_psf_param
      type(field_IO_params), save :: rms_psf_param
      type(field_IO_params), save :: sdev_psf_param
!
      type(time_data), save :: psf_time
      type(ucd_data), save:: psf_ucd
!
      character(len=kchara) :: fname_tmp
!
      integer(kind = kint) :: istep_start, istep_end
      integer(kind = kint) :: istep_int
      real(kind = kreal) :: rmin, rmax
!
      integer(kind = kint) :: istep, icou, nnod_psf, ncomp_phys
      integer(kind = kint) :: inod, nd, i
!
      real(kind = kreal) :: acou
      real(kind = kreal), allocatable :: tave_psf(:,:)
      real(kind = kreal), allocatable :: trms_psf(:,:)
      real(kind = kreal), allocatable :: tsdev_psf(:,:)
!
!  ===========
! . for local 
!  ===========
!
      call input_ucd_file_format_code                                   &
     &   (psf_file_param%iflag_format, psf_file_param%file_prefix)
!
      write(*,*) 'input istep_start, istep_end, istep_int'
      read(*,*) istep_start, istep_end, istep_int
!
      write(*,*) 'input radius range'
      read(*,*) rmin, rmax
!
      fname_tmp = add_int_suffix                                        &
     &          (istep_start, psf_file_param%file_prefix)
      write(ave_psf_param%file_prefix, '(a9,a)')                        &
     &                                'time_ave_', trim(fname_tmp)
      write(rms_psf_param%file_prefix, '(a9,a)')                        &
     &                                'time_rms_', trim(fname_tmp)
      write(sdev_psf_param%file_prefix,'(a9,a)')                        &
     &                                'time_dev_', trim(fname_tmp)
!
      ave_psf_param%iflag_format =  psf_file_param%iflag_format
      rms_psf_param%iflag_format =  psf_file_param%iflag_format
      sdev_psf_param%iflag_format = psf_file_param%iflag_format
!
      call load_psf_data_to_link_IO                                     &
     &   (istep_start, psf_file_param, psf_u, psf_ucd)
      call alloc_psf_averages(psf_u%psf_phys, psf_average)
!
      call sel_write_grd_file(-1, ave_psf_param, psf_ucd)
      call sel_write_grd_file(-1, rms_psf_param, psf_ucd)
      call sel_write_grd_file(-1, sdev_psf_param, psf_ucd)
!
!   Evaluate size of patches
!
      write(*,*) 'allocate_norms_4_psf'
      call allocate_norms_4_psf                                         &
     &   (psf_u%psf_nod, psf_u%psf_ele, psf_normal)
      write(*,*) 'cal_center_ele_4_psf'
      call cal_center_ele_4_psf                                         &
     &   (psf_u%psf_nod, psf_u%psf_ele, psf_normal)
      write(*,*) 'cal_norm_area_4_psf'
      call cal_norm_area_4_psf                                          &
     &   (psf_u%psf_nod, psf_u%psf_ele, psf_normal)
!
      write(*,*) 'set_averaging_range'
      call set_averaging_range(rmin, rmax, psf_normal)
!
      call open_psf_ave_rms_data                                        &
     &   (psf_file_param%file_prefix, psf_u%psf_phys)
      call open_psf_range_data                                          &
     &   (psf_file_param%file_prefix,  psf_u%psf_phys)
!
      nnod_psf = psf_u%psf_nod%numnod
      ncomp_phys = psf_u%psf_phys%ntot_phys
      allocate(tave_psf(nnod_psf,ncomp_phys))
      allocate(trms_psf(nnod_psf,ncomp_phys))
      allocate(tsdev_psf(nnod_psf,ncomp_phys))
      tave_psf =  zero
      trms_psf =  zero
      tsdev_psf = zero
!
      icou = 0
      write(*,'(a,i15)', advance='NO')                                  &
     &          'read for averaging. Step:  ', istep_start
      do istep = istep_start, istep_end, istep_int
        icou = icou + 1
        write(*,'(15a1)', advance='NO') (char(8),i=1,15)
        write(*,'(i15)', advance='NO') istep
!
        call sel_read_udt_file                                          &
     &     (-1, istep, psf_file_param, psf_time, psf_ucd)
        call cal_rms_ave_4_psf(psf_u%psf_ele, psf_u%psf_phys,           &
     &     psf_normal, psf_average)
        call cal_minmax_psf                                             &
     &     (psf_u%psf_nod%numnod, psf_u%psf_phys%ntot_phys,             &
     &      psf_u%psf_phys%d_fld, psf_average)
!
!
!$omp parallel
        do nd = 1, psf_u%psf_phys%ntot_phys
!$omp do
          do inod = 1, psf_u%psf_nod%numnod
            tave_psf(inod,nd) = tave_psf(inod,nd)                       &
     &                         + psf_u%psf_phys%d_fld(inod,nd)
            trms_psf(inod,nd) = trms_psf(inod,nd)                       &
     &                         + psf_u%psf_phys%d_fld(inod,nd)**2
          end do
!$omp end do
        end do
!$omp end parallel
!
        call write_psf_ave_rms_data                                     &
     &     (istep, psf_normal%area, psf_average)
        call write_psf_range_data(istep, psf_average)
      end do
      write(*,*)
      call close_psf_ave_rms_data
      call close_psf_range_data
!
      acou = one / dble(icou)
!$omp parallel
      do nd = 1, psf_u%psf_phys%ntot_phys
!$omp do
        do inod = 1, psf_u%psf_nod%numnod
          tsdev_psf(inod,nd) = trms_psf(inod,nd) - tave_psf(inod,nd)**2
!
          tave_psf(inod,nd) = tave_psf(inod,nd) * acou
          trms_psf(inod,nd) = sqrt(trms_psf(inod,nd) * acou)
          tsdev_psf(inod,nd) = sqrt(tsdev_psf(inod,nd) * acou)
        end do
!$omp end do
      end do
!$omp end parallel
!
!
      call copy_filed_to_phys_data(tave_psf, psf_u%psf_phys)
      call sel_write_udt_file                                           &
     &   (-1, istep_end, ave_psf_param, psf_time, psf_ucd)
!
      call copy_filed_to_phys_data(trms_psf, psf_u%psf_phys)
      call sel_write_udt_file                                           &
     &   (-1, istep_end, rms_psf_param, psf_time, psf_ucd)
!
      call copy_filed_to_phys_data(tsdev_psf, psf_u%psf_phys)
      call sel_write_udt_file                                           &
     &   (-1, istep_end, sdev_psf_param, psf_time, psf_ucd)
!
      stop ' //// program normally finished //// '
!
      end program psf_rms_aves
