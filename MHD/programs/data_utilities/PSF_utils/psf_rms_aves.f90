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
!
      type(ucd_data), save:: psf_ucd

      character(len=kchara) :: fname_tmp
      character(len=kchara) :: psf_ave_header
      character(len=kchara) :: psf_rms_header
      character(len=kchara) :: psf_sdev_header
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
      write(*,*) 'Choose psf format'
      write(*,*) iflag_ucd, ': UCD'
      write(*,*) iflag_udt, ': UDT'
      write(*,*) iflag_vtk, ': VTK'
      write(*,*) iflag_ucd_gz, ': gzipped_UCD'
      write(*,*) iflag_udt_gz, ': gzipped_UDT'
      write(*,*) iflag_vtk_gz, ': gzipped_VTK'
!
      read(*,*)  iflag_psf_fmt
      write(*,*) 'iflag_psf_fmt', iflag_psf_fmt
!
      write(*,*) 'input psf file name'
      read(*,*) psf_file_header
!
      write(*,*) 'input istep_start, istep_end, istep_int'
      read(*,*) istep_start, istep_end, istep_int
!
      write(*,*) 'input radius range'
      read(*,*) rmin, rmax
!
      call add_int_suffix(istep_start, psf_file_header, fname_tmp)
      write(psf_ave_header, '(a9,a)') 'time_ave_', trim(fname_tmp)
      write(psf_rms_header, '(a9,a)') 'time_rms_', trim(fname_tmp)
      write(psf_sdev_header,'(a9,a)') 'time_dev_', trim(fname_tmp)
!
      psf_u%iflag_psf_fmt =   iflag_psf_fmt
      psf_u%psf_file_header = psf_file_header
!
      call load_psf_data_to_link_IO(istep_start, psf_u, psf_ucd)
      call alloc_psf_averages(psf_u%psf_phys, psf_average)
!
      psf_ucd%file_prefix = psf_ave_header
      call sel_write_grd_file(iminus, psf_ucd)
      psf_ucd%file_prefix = psf_rms_header
      call sel_write_grd_file(iminus, psf_ucd)
      psf_ucd%file_prefix = psf_sdev_header
      call sel_write_grd_file(iminus, psf_ucd)
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
      call open_psf_ave_rms_data(psf_file_header, psf_u%psf_phys)
      call open_psf_range_data(psf_file_header,  psf_u%psf_phys)
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
      psf_ucd%ifmt_file = iflag_psf_fmt
      psf_ucd%file_prefix = psf_file_header
!
      icou = 0
      write(*,'(a,i15)', advance='NO')                                  &
     &          'read for averaging. Step:  ', istep_start
      do istep = istep_start, istep_end, istep_int
        icou = icou + 1
        write(*,'(15a1)', advance='NO') (char(8),i=1,15)
        write(*,'(i15)', advance='NO') istep
!
        call sel_read_udt_file(iminus, istep, psf_ucd)
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
          tave_psf(inod,nd) = tave_psf(inod,nd) * acou
          trms_psf(inod,nd) = sqrt(trms_psf(inod,nd) * acou)
        end do
!$omp end do
      end do
!$omp end parallel
!
!
      icou = 0
      write(*,'(a,i15)', advance='NO')                                &
     &          'read for RMS. Step:  ', istep
      do istep = istep_start, istep_end, istep_int
        icou = icou + 1
        write(*,'(15a1)', advance='NO') (char(8),i=1,15)
        write(*,'(i15)', advance='NO') istep
!
        call sel_read_udt_file(iminus, istep, psf_ucd)
!
!$omp parallel
        do nd = 1, psf_u%psf_phys%ntot_phys
!$omp do
          do inod = 1, psf_u%psf_nod%numnod
          tsdev_psf(inod,nd) = tsdev_psf(inod,nd)                       &
     &                        + (psf_u%psf_phys%d_fld(inod,nd)          &
     &                         - tave_psf(inod,nd))**2
          end do
!$omp end do
        end do
!$omp end parallel
      end do
      write(*,*)
!
!$omp parallel
      do nd = 1, psf_u%psf_phys%ntot_phys
!$omp do
        do inod = 1, psf_u%psf_nod%numnod
          tsdev_psf(inod,nd) = sqrt(tsdev_psf(inod,nd) * acou)
        end do
!$omp end do
      end do
!$omp end parallel
!
!
      psf_ucd%file_prefix = psf_ave_header
      call copy_filed_to_phys_data(tave_psf, psf_u%psf_phys)
      call sel_write_udt_file(iminus, istep_end, psf_ucd)
!
!
      psf_ucd%file_prefix = psf_rms_header
      call copy_filed_to_phys_data(trms_psf, psf_u%psf_phys)
      call sel_write_udt_file(iminus, istep_end, psf_ucd)
!
      psf_ucd%file_prefix = psf_sdev_header
      call copy_filed_to_phys_data(tsdev_psf, psf_u%psf_phys)
      call sel_write_udt_file(iminus, istep_end, psf_ucd)
!
      stop ' //// program normally finished //// '
!
      end program psf_rms_aves
