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
      use set_parallel_file_name
      use read_psf_select_4_zlib
      use cal_psf_rms_aves
      use take_avarages_4_psf
!
      implicit    none
!
      character(len=kchara) :: psf_org_header
      integer(kind = kint) :: iflag_psf_fmt = iflag_udt
!
      character(len=kchara) :: fname_tmp
      character(len=kchara) :: psf_ave_header
      character(len=kchara) :: psf_rms_header
      character(len=kchara) :: psf_sdev_header
!
      integer(kind = kint) :: istep_start, istep_end
      integer(kind = kint) :: istep_int
!
      integer(kind = kint) :: istep, icou
      integer(kind = kint) :: inod, nd
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
      write(*,*) 'input psf file name'
      read(*,*) psf_org_header
!
      write(*,*) 'inputistep_start, istep_end, istep_int'
      read(*,*) istep_start, istep_end, istep_int
!
      call add_int_suffix(istep_start, psf_org_header, fname_tmp)
      write(psf_ave_header, '(a5,a)') 'tave_', trim(fname_tmp)
      write(psf_rms_header, '(a5,a)') 'trms_', trim(fname_tmp)
      write(psf_sdev_header,'(a5,a)') 'sdev_', trim(fname_tmp)
!
!
      call input_psf_result(iflag_psf_fmt, psf_org_header, istep_start)
!
      call sel_write_psf_grd_file(iflag_psf_fmt, psf_ave_header)
      call sel_write_psf_grd_file(iflag_psf_fmt, psf_rms_header)
      call sel_write_psf_grd_file(iflag_psf_fmt, psf_sdev_header)
!
!   Evaluate size of patches
!
      call open_psf_ave_rms_data
!
      allocate( tave_psf(numnod_psf,ncomptot_psf) )
      allocate( trms_psf(numnod_psf,ncomptot_psf) )
      allocate( tsdev_psf(numnod_psf,ncomptot_psf) )
      tave_psf =  zero
      trms_psf =  zero
      tsdev_psf = zero
!
      icou = 0
      do istep = istep_start, istep_end, istep_int
        icou = icou + 1
        call sel_read_psf_udt_file(iflag_psf_fmt,                       &
     &       psf_org_header, istep)
!
        do nd = 1, ncomptot_psf
          do inod = 1, numnod_psf
            tave_psf(inod,nd) = tave_psf(inod,nd) + d_nod_psf(inod,nd)
            trms_psf(inod,nd) = trms_psf(inod,nd)                       &
     &                          + d_nod_psf(inod,nd)**2
          end do
        end do
!
        call cal_psf_ave_rms_data(istep)
      end do
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
      do istep = istep_start, istep_end, istep_int
        icou = icou + 1
        call sel_read_psf_udt_file(iflag_psf_fmt,                       &
     &       psf_org_header, istep)
!
        do nd = 1, ncomptot_psf
          tsdev_psf(1:numnod_psf,nd) = tsdev_psf(1:numnod_psf,nd)       &
     &                                + (d_nod_psf(1:numnod_psf,nd)     &
     &                                 - tave_psf(1:numnod_psf,nd))**2
        end do
      end do
!
      do nd = 1, ncomptot_psf
        tsdev_psf(1:numnod_psf,nd)                                      &
     &         = sqrt(tsdev_psf(1:numnod_psf,nd) * acou)
      end do
!
!
!
      do nd = 1, ncomptot_psf
        d_nod_psf(1:numnod_psf,nd) = tave_psf(1:numnod_psf,nd)
      end do
      call sel_write_psf_udt_file(iflag_psf_fmt, psf_ave_header,        &
     &    istep_end)
!
      do nd = 1, ncomptot_psf
        d_nod_psf(1:numnod_psf,nd) = trms_psf(1:numnod_psf,nd)
      end do
      call sel_write_psf_udt_file(iflag_psf_fmt, psf_rms_header,        &
     &    istep_end)
!
      do nd = 1, ncomptot_psf
        d_nod_psf(1:numnod_psf,nd) = tsdev_psf(1:numnod_psf,nd)
      end do
      call sel_write_psf_udt_file(iflag_psf_fmt, psf_sdev_header,       &
     &    istep_end)
!
      stop ' //// program normally terminated //// '
!
      end program psf_rms_aves
