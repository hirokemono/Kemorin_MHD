!
!     program psf_fix_reynods_work
!
!      program for pick up surface connectivities form subdomain mesh
!         programmed  by  H.Matsui (U. Chicago)  on Oct. 2003 (ver 1.0)
!         Modified  by  H.Matsui (U. Chicago)  on Jan. 2007 (ver 2.0)
!
      program psf_fix_reynods_work
!
      use m_precision
      use m_constants
!
      use m_psf_results
      use m_field_file_format
      use m_phys_labels
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
      type(field_IO_params), save :: ave_psf_param
!
      type(time_data), save :: psf_time
      type(ucd_data), save:: psf_ucd
!
      character(len=kchara) :: fname_tmp
!
      integer(kind = kint) :: istep_start, istep_end
      integer(kind = kint) :: istep_int
!
      integer(kind = kint) :: istep, icou, nnod_psf, ncomp_phys
      integer(kind = kint) :: ifld, ist, ied, inod, nd, i
!
      real(kind = kreal), allocatable :: tave_psf(:,:)
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
      fname_tmp = add_int_suffix                                        &
     &          (istep_start, psf_file_param%file_prefix)
      write(ave_psf_param%file_prefix, '(a,a6)')                        &
     &                                trim(fname_tmp), '_fixed'
!
      ave_psf_param%iflag_format =  psf_file_param%iflag_format
!
      call load_psf_data_to_link_IO                                     &
     &   (istep_start, psf_file_param, psf_u, psf_ucd)
!
      call sel_write_grd_file(iminus, ave_psf_param, psf_ucd)
!
!   Evaluate size of patches
!
      nnod_psf = psf_u%psf_nod%numnod
      ncomp_phys = psf_u%psf_phys%ntot_phys
      allocate(tave_psf(nnod_psf,ncomp_phys))
      tave_psf =  zero
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
     &     (iminus, istep, psf_file_param, psf_time, psf_ucd)
!
!
        do ifld = 1, psf_u%psf_phys%num_phys
          if(psf_u%psf_phys%phys_name(ifld) .ne. fhd_Reynolds_work)     &
     &     cycle
!
          ist = psf_u%psf_phys%istack_component(ifld-1) + 1
          ied = psf_u%psf_phys%istack_component(ifld)
          write(*,*) 'Fix field at', ifld, ist, ied
          do nd = ist, ied
!$omp parallel do
            do inod = 1, psf_u%psf_nod%numnod
              psf_u%psf_phys%d_fld(inod,nd)                             &
     &                          = - psf_u%psf_phys%d_fld(inod,nd)
            end do
!$omp end parallel do
          end do
        end do
!
        call sel_write_udt_file                                         &
     &     (iminus, istep, ave_psf_param, psf_time, psf_ucd)
      end do
!
      stop ' //// program normally finished //// '
!
      end program psf_fix_reynods_work
