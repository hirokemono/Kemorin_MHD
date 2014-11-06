!
!     program psf_rms_aves
!
      program psf_rms_aves
!
!      program for pick up surface connectivities form subdomain mesh
!         programmed  by  H.Matsui (U. Chicago)  on Oct. 2003 (ver 1.0)
!         Modified  by  H.Matsui (U. Chicago)  on Jan. 2007 (ver 2.0)
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
      use load_psf_data
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
      integer(kind = kint) :: istep, icou
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
!
      call s_load_psf_data(istep_start)
      call set_psf_mesh_to_ucd_data(psf_ucd)
!
      psf_ucd%ifmt_file = iflag_udt
      psf_ucd%file_prefix = psf_ave_header
      call sel_write_grd_file(iminus, psf_ucd)
      psf_ucd%file_prefix = psf_rms_header
      call sel_write_grd_file(iminus, psf_ucd)
      psf_ucd%file_prefix = psf_sdev_header
      call sel_write_grd_file(iminus, psf_ucd)
!
!   Evaluate size of patches
!
      call allocate_norms_4_psf
      call cal_center_ele_4_psf
      call cal_norm_area_4_psf
!
      call set_averaging_range(rmin, rmax)
!
      call open_psf_ave_rms_data(psf_file_header)
!
      allocate( tave_psf(numnod_psf,ncomptot_psf) )
      allocate( trms_psf(numnod_psf,ncomptot_psf) )
      allocate( tsdev_psf(numnod_psf,ncomptot_psf) )
      tave_psf =  zero
      trms_psf =  zero
      tsdev_psf = zero
!
      psf_ucd%ifmt_file = iflag_psf_fmt
      psf_ucd%file_prefix = psf_file_header
!
      icou = 0
      write(*,'(a,i10)', advance='NO')                                  &
     &          'read for averaging. Step:  ', istep_start
      do istep = istep_start, istep_end, istep_int
        icou = icou + 1
        write(*,'(10a1)', advance='NO') (char(8),i=1,10)
        write(*,'(i10)', advance='NO') istep
!
        call sel_read_udt_file(iminus, istep, psf_ucd)
        call cal_rms_ave_4_psf
        call cal_minmax_psf
!
!
        do nd = 1, ncomptot_psf
          do inod = 1, numnod_psf
            tave_psf(inod,nd) = tave_psf(inod,nd) + d_nod_psf(inod,nd)
            trms_psf(inod,nd) = trms_psf(inod,nd)                       &
     &                          + d_nod_psf(inod,nd)**2
          end do
        end do
!
        call write_psf_ave_rms_data(istep, area_total_psf)
      end do
      write(*,*)
      call close_psf_ave_rms_data
!
      acou = one / dble(icou)
      do nd = 1, ncomptot_psf
        do inod = 1, numnod_psf
          tave_psf(inod,nd) = tave_psf(inod,nd) * acou
          trms_psf(inod,nd) = sqrt(trms_psf(inod,nd) * acou)
        end do
      end do
!
!
      icou = 0
      write(*,'(a,i10)', advance='NO')                                &
     &          'read for RMS. Step:  ', istep
      do istep = istep_start, istep_end, istep_int
        icou = icou + 1
        write(*,'(10a1)', advance='NO') (char(8),i=1,10)
        write(*,'(i10)', advance='NO') istep
!
        call sel_read_udt_file(iminus, istep, psf_ucd)
!
        do nd = 1, ncomptot_psf
          tsdev_psf(1:numnod_psf,nd) = tsdev_psf(1:numnod_psf,nd)       &
     &                                + (d_nod_psf(1:numnod_psf,nd)     &
     &                                 - tave_psf(1:numnod_psf,nd))**2
        end do
      end do
      write(*,*)
!
      do nd = 1, ncomptot_psf
        tsdev_psf(1:numnod_psf,nd)                                      &
     &         = sqrt(tsdev_psf(1:numnod_psf,nd) * acou)
      end do
!
!
      do nd = 1, ncomptot_psf
        d_nod_psf(1:numnod_psf,nd) = tave_psf(1:numnod_psf,nd)
      end do

      psf_ucd%file_prefix = psf_ave_header
      call sel_write_udt_file(iminus, istep_end, psf_ucd)
!
      do nd = 1, ncomptot_psf
        d_nod_psf(1:numnod_psf,nd) = trms_psf(1:numnod_psf,nd)
      end do
!
      psf_ucd%file_prefix = psf_rms_header
      call sel_write_udt_file(iminus, istep_end, psf_ucd)
!
      do nd = 1, ncomptot_psf
        d_nod_psf(1:numnod_psf,nd) = tsdev_psf(1:numnod_psf,nd)
      end do
      psf_ucd%file_prefix = psf_sdev_header
      call sel_write_udt_file(iminus, istep_end, psf_ucd)
!
      stop ' //// program normally finished //// '
!
      end program psf_rms_aves
