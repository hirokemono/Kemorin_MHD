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
      use m_base_field_labels
      use m_psf_results
      use m_field_file_format
!
      use t_file_IO_parameter
      use t_time_data
      use t_ucd_data
!
      use set_parallel_file_name
      use ucd_IO_select
!
      implicit    none
!
!
      type(field_IO_params), save :: fix_psf_param
      type(time_data), save :: psf_time
      type(ucd_data), save:: psf_ucd
!
      character(len=kchara) :: psf_format
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
      read(*,*) psf_file_param%file_prefix
!
      write(*,*) 'input fixed file name'
      read(*,*) fix_psf_param%file_prefix
!
      write(*,*) 'inputistep_start, istep_end, istep_int'
      read(*,*) istep_start, istep_end, istep_int
!
      call init_field_type_flags
      psf_file_param%iflag_format                                       &
     &      = choose_ucd_file_format(psf_format, ione)
      call dealloc_field_type_flags
!
      call load_psf_data_to_link_IO                                     &
     &   (istep_start, psf_file_param, t_IO_u, psf_u, psf_ucd)
      fix_psf_param%iflag_format = psf_file_param%iflag_format
!
      call sel_write_grd_file(-1, fix_psf_param, psf_ucd)
!
      ipsf_temp = 0
      do ifld = 1, psf_u%psf_phys%num_phys
        if(psf_u%psf_phys%phys_name(ifld) .eq. temperature%name) then
          ipsf_temp = psf_u%psf_phys%istack_component(ifld-1) + 1
          exit
        end if
      end do
!
      allocate( reftemp_psf(psf_u%psf_nod%numnod) )
      reftemp_psf =  zero
!
      do inod = 1, psf_u%psf_nod%numnod
        r = sqrt(psf_u%psf_nod%xx(inod,1)**2                            &
     &         + psf_u%psf_nod%xx(inod,2)**2                            &
     &         + psf_u%psf_nod%xx(inod,3)**2)
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
      write(*,'(a,i15)', advance='NO')                                  &
     &          'read for correctiong. Step:  ', istep_start
      do istep = istep_start, istep_end, istep_int
        write(*,'(10a1)', advance='NO') (char(8),i=1,10)
        write(*,'(i15)', advance='NO') istep
!
        call sel_read_udt_file                                          &
     &     (-1, istep, psf_file_param, psf_time, psf_ucd)
!
        do inod = 1, psf_u%psf_nod%numnod
          psf_u%psf_phys%d_fld(inod,ipsf_temp)                          &
     &         = psf_u%psf_phys%d_fld(inod,ipsf_temp)                   &
     &          + reftemp_psf(inod)
        end do
!
        call sel_write_udt_file                                         &
     &     (-1, istep, fix_psf_param, psf_time, psf_ucd)
      end do
      write(*,*)
!
      stop ' //// program normally finished //// '
!
      end program fix_temperature_4_psf
