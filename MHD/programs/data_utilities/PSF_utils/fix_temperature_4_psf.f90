!fix_temperature_4_psf.f90
!
!     program fix_temperature_4_psf
!
!      program to fix perturbation temperature to temperature
!         programmed  by  H.Matsui (U. Chicago)  on Oct. 2003 (ver 1.0)
!         Modified  by  H.Matsui (U. Chicago)  on Jan. 2007 (ver 2.0)
!
      program fix_temperature_4_psf
!
      use m_precision
      use m_constants
!
      use m_phys_labels
      use m_psf_results
      use m_field_file_format
!
      use t_ucd_data
!
      use set_parallel_file_name
      use ucd_IO_select
      use read_psf_select_4_zlib
!
      implicit    none
!
!
      type(ucd_data), save:: psf_ucd
!
      character(len=kchara) :: psf_format
      character(len=kchara) :: psf_org_header
      character(len=kchara) :: psf_fixed_header
!
      integer(kind = kint) :: istep_start, istep_end
      integer(kind = kint) :: istep_int
!
      integer(kind = kint) :: ipsf_temp
      integer(kind = kint) :: istep
      integer(kind = kint) :: inod, i, ifld
!
      real(kind = kreal) :: r
      real(kind = kreal), parameter :: high_temp = one
      real(kind = kreal), parameter :: low_temp =  zero
      real(kind = kreal), parameter :: depth_high_t = 7.0d0 / 13.0d0
      real(kind = kreal), parameter :: depth_low_t = depth_high_t + one
!
      real(kind = kreal), allocatable :: reftemp_psf(:)
!
!  ===========
! . for local 
!  ===========
!
      write(*,*) 'input psf file format'
      read(*,*) psf_format
!
      write(*,*) 'input original psf file name'
      read(*,*) psf_org_header
!
      write(*,*) 'input fixed file name'
      read(*,*) psf_fixed_header
!
      write(*,*) 'inputistep_start, istep_end, istep_int'
      read(*,*) istep_start, istep_end, istep_int
!
      call choose_ucd_file_format(psf_format, ione, iflag_psf_fmt)
      psf_ucd%ifmt_file = iflag_psf_fmt
!
      psf_file_header = psf_org_header
      psf_ucd%file_prefix = psf_org_header
      call sel_read_alloc_psf_file(psf_ucd%ifmt_file, istep_start)
      call set_psf_mesh_to_ucd_data(psf_ucd)
!
      psf_ucd%file_prefix = psf_fixed_header
      call sel_write_grd_file(-1, psf_ucd)
!
      ipsf_temp = 0
      do ifld = 1, nfield_psf
        if(psf_data_name(ifld) .eq. fhd_temp) then
          ipsf_temp = istack_comp_psf(ifld-1) + 1
          exit
        end if
      end do
!
      allocate( reftemp_psf(numnod_psf) )
      reftemp_psf =  zero
!
      do inod = 1, numnod_psf
        r = sqrt(xx_psf(inod,1)**2 + xx_psf(inod,2)**2                  &
     &     + xx_psf(inod,3)**2)
        reftemp_psf(inod) = ( (high_temp - low_temp)                    &
     &                        * depth_high_t*depth_low_t / r            &
     &                        - depth_high_t*high_temp                  &
     &                        + depth_low_t* low_temp )                 &
     &                         / ( depth_low_t - depth_high_t )
      end do
!
!   Evaluate size of patches
!
!
      write(*,'(a,i10)', advance='NO')                                  &
     &          'read for correctiong. Step:  ', istep_start
      do istep = istep_start, istep_end, istep_int
        write(*,'(10a1)', advance='NO') (char(8),i=1,10)
        write(*,'(i10)', advance='NO') istep
!
        psf_ucd%file_prefix = psf_org_header
        call sel_read_udt_file(-1, istep, psf_ucd)
!
        do inod = 1, numnod_psf
          d_nod_psf(inod,ipsf_temp) = d_nod_psf(inod,ipsf_temp)         &
      &                              + reftemp_psf(inod)
        end do
!
        psf_ucd%file_prefix = psf_fixed_header
        call sel_write_udt_file(-1, istep, psf_ucd)
      end do
      write(*,*)
!
      stop ' //// program normally terminated //// '
!
      end program fix_temperature_4_psf
